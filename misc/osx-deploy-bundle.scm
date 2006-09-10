;;;; osx-deploy-bundle.scm
;
; Use like this:
;
; % csc <your-application-main-module> -prologue osx-deploy-bundle.scm -framework CoreFoundation


(use posix easyffi)

#>
#include <CoreFoundation/CoreFoundation.h>
<#

(foreign-parse/declare #<<EOF
static char *get_bundle_path()
{
  CFBundleRef bundle = CFBundleGetMainBundle();
  CFURLRef url = CFBundleCopyExecutableURL(bundle);
  static char buffer[ 256 ];
  
  if(CFURLGetFileSystemRepresentation(url, true, buffer, sizeof(buffer))) return buffer;
  else return NULL;
}
EOF
)

(let ((application-path (get_bundle_path)))
  (assert application-path "unable to compute executable path")
  (repository-path (pathname-directory application-path) ) )
