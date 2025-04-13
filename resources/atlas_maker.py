import os
import sys
from PIL import Image
import re
from collections import defaultdict

MIN_ATLAS_WIDTH = 256
MAX_ATLAS_WIDTH = 16384
OUTPUT_DIR = "."
ATLAS_FILE = "images/tileset.png"
METADATA_FILE = "files/tileset.txt"
DEFAULT_FRAMERATE = 12
METADATA_FILENAME = "metadata.txt"

def extract_framerate_from_name(name):
    match = re.search(r'(.*?)(\d+)$', name)
    if match:
        clean_name = match.group(1)
        framerate = int(match.group(2))
        return clean_name, framerate
    return name, None

class MetadataItem:
    def __init__(self, item_type, animation_name, data):
        self.item_type = item_type  # "hitbox" or "reference"
        self.animation_name = animation_name
        self.data = data  # Additional data specific to the item type

class HitboxData(MetadataItem):
    def __init__(self, animation_name, start_frame, boxtype, x, y, width, height):
        super().__init__("hitbox", animation_name, {
            "start_frame": int(start_frame),
            "boxtype": boxtype,
            "x": int(x),
            "y": int(y),
            "width": int(width),
            "height": int(height)
        })

    def to_metadata_string(self):
        d = self.data
        return f"hitbox:{self.animation_name}:{d['start_frame']}:{d['boxtype']}:{d['x']}:{d['y']}:{d['width']}:{d['height']}"

class ReferencePointData(MetadataItem):
    def __init__(self, animation_name, x, y):
        super().__init__("reference", animation_name, {
            "x": int(x),
            "y": int(y)
        })

    def to_metadata_string(self):
        d = self.data
        return f"reference:{self.animation_name}:{d['x']}:{d['y']}"

class AtlasItem:
    def __init__(self, name):
        self.name = name
        self.x = 0
        self.y = 0
        self.metadata_items = []  # For hitboxes and reference points

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

def parse_metadata_file(filepath, item_name):
    """Parse a metadata.txt file and return metadata items."""
    metadata_items = []

    if not os.path.exists(filepath):
        return metadata_items

    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            parts = line.split(':')
            if len(parts) < 2:
                continue

            item_type = parts[0]

            if item_type == "hitbox":
                # Check if we have enough parts for a hitbox
                if len(parts) >= 7:  # Minimum required for either format
                    _, placeholder, *rest = parts
                    
                    # Try to determine if format has start_frame by checking if 3rd field is numeric
                    try:
                        start_frame = int(rest[0])
                        has_start_frame = True
                    except (ValueError, IndexError):
                        has_start_frame = False
                    
                    if has_start_frame and len(rest) >= 5:
                        # Format: hitbox:[name]:start_frame:boxtype:x:y:width:height
                        start_frame = int(rest[0])
                        boxtype = rest[1]
                        x, y, width, height = int(rest[2]), int(rest[3]), int(rest[4]), int(rest[5])
                        
                        if placeholder == "[item]" or placeholder == "[name]":
                            animation_name = item_name
                        else:
                            animation_name = placeholder
                            
                        metadata_items.append(HitboxData(
                            animation_name, start_frame, boxtype, x, y, width, height
                        ))
                    elif not has_start_frame and len(rest) >= 4:
                        # Format: hitbox:characters/ink_master/jump/jumping:vulnerable:193:155:156:326
                        boxtype = rest[0]
                        x, y, width, height = int(rest[1]), int(rest[2]), int(rest[3]), int(rest[4])
                        
                        if placeholder == "[item]" or placeholder == "[name]":
                            animation_name = item_name
                        else:
                            animation_name = placeholder
                            
                        # Use a default start_frame of 0 for this format
                        metadata_items.append(HitboxData(
                            animation_name, 0, boxtype, x, y, width, height
                        ))
            
            elif item_type == "reference" and len(parts) >= 4:
                # Format: reference:[item]:x:y
                # [item] should be replaced with the animation name
                _, placeholder, x, y = parts[:4]
                
                # Replace [item] with the actual animation path
                if placeholder == "[item]" or placeholder == "[name]":
                    animation_name = item_name
                else:
                    animation_name = placeholder
                    
                metadata_items.append(ReferencePointData(
                    animation_name, x, y
                ))

    return metadata_items

def collect_assets(root_dir):
    static_images = []
    animations = []
    animation_dirs = []  # Added this missing line

    for dirpath, dirnames, filenames in os.walk(root_dir):
        png_files = [f for f in filenames if f.lower().endswith('.png')]
        metadata_filepath = os.path.join(dirpath, METADATA_FILENAME)

        if is_animation_directory(dirpath, png_files):
            # Process animation directory
            animation_dirs.append((dirpath, png_files))

            # Get animation name from relative path
            rel_path = os.path.relpath(dirpath, start=root_dir)
            raw_name = rel_path.replace(os.path.sep, '/')
            clean_name, custom_framerate = extract_framerate_from_name(raw_name)
            framerate = custom_framerate if custom_framerate is not None else DEFAULT_FRAMERATE

            # Sort frames by number
            def extract_number(filename):
                digits = ''.join(c for c in filename if c.isdigit())
                return int(digits) if digits else 0

            png_files.sort(key=extract_number)
            frame_paths = [os.path.join(dirpath, f) for f in png_files]

            # Create animation sequence
            anim = AnimationSequence(clean_name, frame_paths, framerate=framerate)

            # Add metadata if exists
            if os.path.exists(metadata_filepath):
                anim.metadata_items = parse_metadata_file(metadata_filepath, clean_name)

            animations.append(anim)
        else:
            # Process static images
            parent_metadata = None
            if os.path.exists(metadata_filepath):
                parent_metadata = parse_metadata_file(metadata_filepath, "")

            for filename in png_files:
                filepath = os.path.join(dirpath, filename)
                rel_path = os.path.relpath(filepath, start=root_dir)
                name = rel_path.replace(os.path.sep, '/').replace('.png', '')

                static_img = StaticTexture(name, filepath)

                # Add metadata if parent metadata exists
                if parent_metadata:
                    # Filter metadata items for this specific image
                    image_name = os.path.splitext(filename)[0]
                    full_path_name = name  # This is the full path without extension
                    
                    for item in parent_metadata:
                        # Check if the item applies to this image by:
                        # 1. Empty animation_name (applies to all)
                        # 2. Matches just the filename
                        # 3. Matches the full path
                        if (item.animation_name == "" or 
                            item.animation_name == image_name or 
                            item.animation_name == full_path_name):
                            
                            # Clone the item with the correct name
                            if item.item_type == "hitbox":
                                static_img.metadata_items.append(HitboxData(
                                    name,
                                    item.data["start_frame"],
                                    item.data["boxtype"],
                                    item.data["x"],
                                    item.data["y"],
                                    item.data["width"],
                                    item.data["height"]
                                ))
                            elif item.item_type == "reference":
                                static_img.metadata_items.append(ReferencePointData(
                                    name,
                                    item.data["x"],
                                    item.data["y"]
                                ))

                static_images.append(static_img)

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

    # Add animation and texture metadata
    for anim in animations:
        metadata.append(f"anim:{anim.name}:{anim.x}:{anim.y}:{anim.width}:{anim.height}:{anim.frame_count}:{anim.framerate}")
        # Add hitbox and reference point data
        for item in anim.metadata_items:
            # Make sure the animation name is properly set for this metadata item
            if item.item_type == "hitbox":
                hitbox_str = item.to_metadata_string()
                # Replace [name] or [item] placeholders with the actual animation name
                hitbox_str = hitbox_str.replace("hitbox:[name]:", f"hitbox:{anim.name}:")
                hitbox_str = hitbox_str.replace("hitbox:[item]:", f"hitbox:{anim.name}:")
                metadata.append(hitbox_str)
            elif item.item_type == "reference":
                ref_str = item.to_metadata_string()
                # Replace [name] or [item] placeholders with the actual animation name
                ref_str = ref_str.replace("reference:[name]:", f"reference:{anim.name}:")
                ref_str = ref_str.replace("reference:[item]:", f"reference:{anim.name}:")
                metadata.append(ref_str)

    for img in static_images:
        metadata.append(f"tile:{img.name}:{img.x}:{img.y}:{img.width}:{img.height}")
        # Add hitbox and reference point data
        for item in img.metadata_items:
            # Make sure the image name is properly set for this metadata item
            if item.item_type == "hitbox":
                hitbox_str = item.to_metadata_string()
                # Replace [name] or [item] placeholders with the actual image name
                hitbox_str = hitbox_str.replace("hitbox:[name]:", f"hitbox:{img.name}:")
                hitbox_str = hitbox_str.replace("hitbox:[item]:", f"hitbox:{img.name}:")
                metadata.append(hitbox_str)
            elif item.item_type == "reference":
                ref_str = item.to_metadata_string()
                # Replace [name] or [item] placeholders with the actual image name
                ref_str = ref_str.replace("reference:[name]:", f"reference:{img.name}:")
                ref_str = ref_str.replace("reference:[item]:", f"reference:{img.name}:")
                metadata.append(ref_str)

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
    os.makedirs(os.path.dirname(atlas_path), exist_ok=True)
    atlas.save(atlas_path)
    print(f"Saved atlas to {atlas_path}")

    metadata_path = os.path.join(OUTPUT_DIR, METADATA_FILE)
    os.makedirs(os.path.dirname(metadata_path), exist_ok=True)
    with open(metadata_path, 'w') as f:
        for line in metadata:
            f.write(line + '\n')
    print(f"Saved metadata to {metadata_path}")

    print("Done!")
    return 0

if __name__ == "__main__":
    sys.exit(main())
