#include <sys/stat.h>
void updatePlaylists(const char *path, const char *baseDirectory) {

  struct stat pathStat;
  stat(path, &pathStat);

  if (S_ISDIR(pathStat.st_mode)) {
  }

  char
}
