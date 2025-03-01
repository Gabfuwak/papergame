import os
import sys
from PIL import Image
import re
from collections import defaultdict

MIN_ATLAS_WIDTH = 256
MAX_ATLAS_WIDTH = 16384 
OUTPUT_DIR = "."
ATLAS_FILE = "tileset.png"
METADATA_FILE = "tileset.txt"
DEFAULT_FRAMERATE = 12

class AtlasItem:
    def __init__(self, name):
        self.name = name
        self.x = 0
        self.y = 0

class StaticTexture(AtlasItem):
    def __init__(self, name, path, image=None):
        super().__init__(name)
        self.path = path
        if image:
            self.image = image
        else:
            self.image = Image.open(path)
        self.width, self.height = self.image.size

class AnimationSequence(AtlasItem):
    def __init__(self, name, frame_paths, framerate=DEFAULT_FRAMERATE):
        super().__init__(name)
        self.frame_paths = frame_paths
        self.frames = []
        self.width = 0
        self.height = 0
        self.frame_count = len(frame_paths)
        self.framerate = framerate
        
        for path in frame_paths:
            img = Image.open(path)
            self.frames.append(img)
            self.width = max(self.width, img.width)
            self.height = max(self.height, img.height)
        
        for i, img in enumerate(self.frames):
            if img.width != self.width or img.height != self.height:
                print(f"Resizing frame {i} of {name} to match dimensions {self.width}x{self.height}")
                self.frames[i] = img.resize((self.width, self.height))

def is_animation_directory(dirpath, filenames):
    # Look for files with pattern nameXX.png
    return any(re.search(r'\d+\.png$', f) for f in filenames)

def collect_assets(root_dir):
    static_images = []
    animations = []
    animation_dirs = []
    
    for dirpath, dirnames, filenames in os.walk(root_dir):
        png_files = [f for f in filenames if f.lower().endswith('.png')]
        
        if is_animation_directory(dirpath, png_files):
            animation_dirs.append((dirpath, png_files))
        else:
            for filename in png_files:
                filepath = os.path.join(dirpath, filename)
                rel_path = os.path.relpath(filepath, start=root_dir)
                name = rel_path.replace(os.path.sep, '_').replace('.png', '')
                
                static_images.append(StaticTexture(name, filepath))
    
    for dirpath, png_files in animation_dirs:
        def extract_number(filename):
            digits = ''.join(c for c in filename if c.isdigit())
            return int(digits) if digits else 0
        
        png_files.sort(key=extract_number)
        
        frame_paths = [os.path.join(dirpath, f) for f in png_files]
        
        rel_path = os.path.relpath(dirpath, start=root_dir)
        name = rel_path.replace(os.path.sep, '_')
        
        animations.append(AnimationSequence(name, frame_paths))
    
    return static_images, animations

def determine_atlas_width(animations):
    # Find the width needed for the widest animation
    max_anim_width = 0
    for anim in animations:
        anim_width = anim.width * anim.frame_count
        max_anim_width = max(max_anim_width, anim_width)
    
    if max_anim_width < MIN_ATLAS_WIDTH:
        return MIN_ATLAS_WIDTH
    
    width = MIN_ATLAS_WIDTH
    while width < max_anim_width and width < MAX_ATLAS_WIDTH:
        width *= 2 # we round to nearest power of 2 because it looks great to me uwu
    
    return width

def create_atlas(static_images, animations):
    atlas_width = determine_atlas_width(animations)
    print(f"Using atlas width of {atlas_width}px")
    
    # each animation gets a row
    current_y = 0
    for anim in animations:
        # calculate total width needed for this animation
        total_width = anim.width * anim.frame_count
        
        if total_width > atlas_width:
            print(f"Warning: Animation {anim.name} is too wide ({total_width}px) for atlas width ({atlas_width}px)")
            # For now, we'll just let it overflow TODO: add wrapping, but should rarely be a problem
        
        anim.x = 0
        anim.y = current_y
        
        current_y += anim.height
    
    current_x = 0
    current_row_height = 0
    
    for img in static_images:
        if current_x + img.width > atlas_width:
            current_x = 0
            current_y += current_row_height
            current_row_height = 0
        
        img.x = current_x
        img.y = current_y
        
        current_x += img.width
        current_row_height = max(current_row_height, img.height)
    
    atlas_height = current_y + current_row_height

    atlas = Image.new('RGBA', (atlas_width, atlas_height), (0, 0, 0, 0))
    
    for anim in animations:
        for i, frame in enumerate(anim.frames):
            atlas.paste(frame, (anim.x + i * anim.width, anim.y))
    
    for img in static_images:
        atlas.paste(img.image, (img.x, img.y))
    
    metadata = []
    
    for anim in animations:
        metadata.append(f"anim:{anim.name}:{anim.x}:{anim.y}:{anim.width}:{anim.height}:{anim.frame_count}:{anim.framerate}")
    
    for img in static_images:
        metadata.append(f"tile:{img.name}:{img.x}:{img.y}:{img.width}:{img.height}")
    
    return atlas, metadata

def main():
    if len(sys.argv) < 2:
        print("Usage: python atlas_generator.py <asset_directory>")
        return 1
    
    root_dir = sys.argv[1]
    if not os.path.isdir(root_dir):
        print(f"Error: {root_dir} is not a directory")
        return 1
    
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    print(f"Collecting assets from {root_dir}...")
    static_images, animations = collect_assets(root_dir)
    
    print(f"Found {len(static_images)} static images and {len(animations)} animations")
    
    print("Creating texture atlas...")
    atlas, metadata = create_atlas(static_images, animations)
    
    atlas_path = os.path.join(OUTPUT_DIR, ATLAS_FILE)
    atlas.save(atlas_path)
    print(f"Saved atlas to {atlas_path}")
    
    metadata_path = os.path.join(OUTPUT_DIR, METADATA_FILE)
    with open(metadata_path, 'w') as f:
        for line in metadata:
            f.write(line + '\n')
    print(f"Saved metadata to {metadata_path}")
    
    print("Done!")
    return 0

if __name__ == "__main__":
    sys.exit(main())
