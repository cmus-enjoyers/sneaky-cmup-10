use std::fs;
use std::fs::{read_to_string, File};
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn main() {
    let music_path = Path::new("../../lol");
    let config_path = Path::new("../../.config/cmup/playlist");
    let _asdf = update_playlists(music_path, config_path);
}

fn remove_garbage(file: &Path) -> io::Result<()> {
    if !file.exists() {
        return Ok(());
    }
    let mut f = File::open(file)?;
    let mut result = Vec::new();

    for line in read_to_string(file).unwrap().lines() {
        let line_path = Path::new("").join(line);
        if line_path.exists() {
            result.push(line.to_string())
        }
    }
    f.set_len(0)?;
    let _ = f.write_all(result.concat().as_bytes());

    return Ok(());
}

fn change_file_name(path: impl AsRef<Path>) -> PathBuf {
    let mut result = path.as_ref().to_owned();
    if path.as_ref().to_str().unwrap().ends_with("$") {
        let new_parent_dir_name = path.as_ref().to_string_lossy().replace("$", "");
        result.set_file_name(new_parent_dir_name);
    } else {
        let path = path.as_ref();
        let mut result = path.to_owned();
        result.set_file_name(path.file_name().unwrap());
    }

    return result;
}

fn update_playlists(path: &Path, config_path: &Path) -> io::Result<()> {
    if path.ends_with("/.") {
        return Ok(());
    }

    if path.is_dir() {
        for dirs in fs::read_dir(path).unwrap() {
            let new_path = Path::new("").join(dirs.unwrap().path());
            let _asd = update_playlists(new_path.as_path(), config_path);
        }
        return Ok(());
    }

    let parent_dir = change_file_name(path.parent().unwrap().file_name().unwrap());
    let playlist_path = Path::new("").join(config_path);

    eprintln!("like black man {}", path.display());

    let file = File::create(playlist_path.join(parent_dir));
    for _files in fs::read_dir(path.parent().unwrap()).unwrap() {
        let mut convert_path = _files.unwrap().path().to_str().unwrap().as_bytes().to_vec();
        convert_path.push(b'\n');

        if let Err(e) = file.as_ref().unwrap().write_all(&convert_path) {
            eprintln!("Failed to copy file: {}", e);
        } else {
            println!("File copied successfully!");
        }
    }
    return Ok(());
}
