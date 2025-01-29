#include <dirent.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

void clearDirectory(const char *directory) {
  DIR *dir = opendir(directory);
  struct dirent *entry;

  if (!dir) {
    perror("opendir");
    return;
  }

  while ((entry = readdir(dir)) != NULL) {
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
      continue;
    }

    char filePath[1024];
    snprintf(filePath, sizeof(filePath), "%s/%s", directory, entry->d_name);
    if (remove(filePath) != 0) {
      perror("remove");
    }
  }

  closedir(dir);
}

void updatePlaylists(const char *path, const char *baseDir) {
  struct stat pathStat;
  stat(path, &pathStat);

  if (strcmp(path, ".") == 0) {
    return;
  }

  if (S_ISDIR(pathStat.st_mode)) {
    DIR *dir = opendir(path);
    struct dirent *entry;

    if (!dir) {
      perror("opendir");
      return;
    }

    while ((entry = readdir(dir)) != NULL) {
      if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
        continue;
      }

      char nextPath[1024];
      snprintf(nextPath, sizeof(nextPath), "%s/%s", path, entry->d_name);
      updatePlaylists(nextPath, baseDir);
    }

    closedir(dir);
    return;
  }

  char groupingDir[1024], parentDir[1024], playlistPath[1024];

  char *pathDup = strdup(path);
  char *dirName = dirname(pathDup);
  strcpy(parentDir, basename(dirName));

  char *dirNameDup = strdup(dirName);
  strcpy(groupingDir, basename(dirname(dirNameDup)));
  free(dirNameDup);

  snprintf(playlistPath, sizeof(playlistPath), "%s/%s", baseDir, parentDir);

  if (parentDir[strlen(parentDir) - 1] == '$') {
    snprintf(playlistPath, sizeof(playlistPath), "%s/%s-%.*s", baseDir,
             groupingDir, (int)strlen(parentDir) - 1, parentDir);
  }

  free(pathDup);

  FILE *playlistFile = fopen(playlistPath, "r");
  if (playlistFile != NULL) {
    char line[1024];
    while (fgets(line, sizeof(line), playlistFile)) {
      line[strcspn(line, "\n")] = 0;
      if (strcmp(line, path) == 0) {
        fclose(playlistFile);
        return;
      }
    }
    fclose(playlistFile);
  }

  playlistFile = fopen(playlistPath, "a");
  if (playlistFile == NULL) {
    perror("fopen");
    return;
  }
  fprintf(playlistFile, "%s\n", path);
  fclose(playlistFile);
}

int main(int argc, char *argv[]) {
  if (argc < 3) {
    fprintf(stderr, "Usage: %s <music_path> <cmus_config_place>\n", argv[0]);
    return 1;
  }

  struct stat st = {0};
  if (stat(argv[2], &st) == -1) {
    if (mkdir(argv[2], 0700) != 0) {
      perror("mkdir");
      return 1;
    }
  } else {
    clearDirectory(argv[2]);
  }

  updatePlaylists(argv[1], argv[2]);
  return 0;
}
