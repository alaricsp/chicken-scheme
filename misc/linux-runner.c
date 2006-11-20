/*
 * getexename.c
 *
 * written by Nicolai Haehnle <prefect_@gmx.net>
 * I hereby release this trivial piece of code to the public domain.
 *
 * The function getexename() returns the filename of the currently loaded
 * executable.
 *
 * Intended use of this function is to facilitate easier packaging of 
 * third-party software for the Linux operating system. The FHS mandates
 * that files that belong to one package are scattered throughout the
 * file system. This works as long as packages are maintained by a 
 * package management program. However, it is impossible for application
 * developers to provide packages for every Linux distribution out there.
 * Finding the file locations is also difficult when an application is
 * installed locally by a user inside her own home directory.
 *
 * The simplest and most straight-forward solution to this problem is to
 * put all files belonging to a package into the same directory. The program
 * executable can then reference the necessary data files by using paths
 * relative to the executable location.
 * To give an example:
 *
 *  A simple game, consisting of an executable and a number of data files
 *  (e.g. images), resides entirely in one directory, with absolute filenames
 *  like this:
 *    /the/path/foogame
 *    /the/path/images/hero.png
 *    /the/path/images/badass.png
 *  The game executable can use getexename() to find its own location, strip
 *  off the last component to get the directory the executable is located in,
 *  and append the relative paths "images/hero.png" and "images/badass.png"
 *  to reference the data files.
 *  The game will be completely position independent. The user is free to 
 *  move it somewhere else in the filesystem, and it will just work; it will
 *  no longer be necessary to change configuration files or even recompile the
 *  executable.
 *
 * If you are concerned about executables showing up in a user's PATH, you 
 * should somehow arrange for symlinks to be made. For example, if 
 * /usr/games/foogame is a symlink to /the/path/foogame, the user can run the
 * game simply by typing "foogame" in the shell (provided that /usr/games is in
 * the user's PATH); since symlinks cannot fool getexename(), the game will
 * still work. (Do note that a hard link will defeat getexename()).
 *
 * Note that while it is possible to reference data files based on the current
 * working directory, this technique only works if the user explicitly sets
 * the CWD to the application's base directory. Therefore, using the executable
 * name as a base is more robust.
 *
 * Also note that while argv[0] can be used as the executable name in many 
 * cases as well, it is easily fooled by symlinks and may not contain an 
 * absolute filename. argv[0] can also be set to something entirely different
 * from the executable filename by the executing process, either delibaretly
 * or by invoking scripts.
 *
 * Note that this function relies on the layout of the /proc file system, so
 * portability is an issue. While I assume that this part of /proc is fairly
 * stable, I have no documentation whatsoever about potential differences
 * between Linux kernel versions in this area.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <sys/types.h>
#include <unistd.h>

#ifndef PROGRAM
# define PROGRAM   "main"
#endif


/*
 * getexename - Get the filename of the currently running executable
 *
 * The getexename() function copies an absolute filename of the currently 
 * running executable to the array pointed to by buf, which is of length size.
 *
 * If the filename would require a buffer longer than size elements, NULL is
 * returned, and errno is set to ERANGE; an application should check for this
 * error, and allocate a larger buffer if necessary.
 *
 * Return value:
 * NULL on failure, with errno set accordingly, and buf on success. The 
 * contents of the array pointed to by buf is undefined on error.
 *
 * Notes:
 * This function is tested on Linux only. It relies on information supplied by
 * the /proc file system.
 * The returned filename points to the final executable loaded by the execve()
 * system call. In the case of scripts, the filename points to the script 
 * handler, not to the script.
 * The filename returned points to the actual exectuable and not a symlink.
 *
 */
char* getexename(char* buf, size_t size)
{
  char linkname[64]; /* /proc/<pid>/exe */
  pid_t pid;
  int ret;
	
  /* Get our PID and build the name of the link in /proc */
  pid = getpid();
	
  if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
    {
      /* This should only happen on large word systems. I'm not sure
	 what the proper response is here.
	 Since it really is an assert-like condition, aborting the
	 program seems to be in order. */
      abort();
    }

	
  /* Now read the symbolic link */
  ret = readlink(linkname, buf, size);
	
  /* In case of an error, leave the handling up to the caller */
  if (ret == -1)
    return NULL;
	
  /* Report insufficient buffer size */
  if (ret >= size)
    {
      errno = ERANGE;
      return NULL;
    }
	
  /* Ensure proper NUL termination */
  buf[ret] = 0;
	
  return buf;
}


int main(int argc, char *argv[], char *envp[])
{
  char* buf, buf2[ 256 ], buf3[ 256 ];
  int size;
  static char *env2[ 1024 ];
  char **ep, *cp;
	
  buf = NULL;
  size = 32; /* Set an initial size estimate */

  for(;;)	
    {
      char* res;
		
      /* Allocate and fill the buffer */
      buf = (char*)malloc(size);
      res = getexename(buf, size);
		
      /* Get out of the loop on success */
      if (res)
	break;
		
      /* Anything but ERANGE indicates a real error */
      if (errno != ERANGE)
	{
	  perror("getexename() failed");
	  free(buf);
	  buf = NULL;
	  break;
	}
		
      /* ERANGE means the buffer was too small. Free the current
	 buffer and retry with a bigger one. */
      free(buf);
      size *= 2;
    }
	
  /* Exit on failure */
  if (buf == NULL)
    return -1;
	
  cp = strrchr(buf, '/');

  if(cp != NULL) *cp = '\0';

  ep = env2; 
  sprintf(buf2, "LD_LIBRARY_PATH=%s", buf);
  *(ep++) = buf2;
  sprintf(buf3, "CHICKEN_REPOSITORY=%s", buf);
  *(ep++) = buf3;
  
  while(*envp != NULL) {
    char *p2 = strchr(*envp, '=');

    if(!strncmp(p2, "CHICKEN_REPOSITORY", strlen("CHICKEN_REPOSITORY")) ||
       !strncmp(p2, "LD_LIBRARY_PATH", strlen("LD_LIBRARY_PATH")))
      ++envp;
    else {
      *ep = *(envp++);
    
      if(*(ep++) == NULL) break;
    }
  }

  *ep = NULL;
  strcat(buf, "/");
  strcat(buf, PROGRAM);

  if(execve(buf, argv + 1, env2) == -1)
    perror("execve failed");
		
  return 0; /* Indicate success */
}
