#include "./headers/clearDirectory.h"
#include <dirent.h>
#include <stdio.h>
#include <string.h>

void clearDirectory(const char *path) {

  DIR *dir = opendir(path);
  struct dirent *entry;

  if (!dir) {
    perror("clearDirectory: opendir");
  }

  while ((entry = readdir(dir)) != NULL) {

    if (strcmp(".", entry->d_name) == 0 || strcmp("..", entry->d_name) == 0) {
      continue;
    }

    char filePath[1024];
    snprintf(filePath, sizeof(filePath), "%s/%s", path, entry->d_name);

    if (remove(filePath) != 0) {
      perror("clearDirectory: remove");
    }
  }
}
