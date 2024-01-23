!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 17, 2003
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/17/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME icdiag04t
#define DT_DECL
#if (defined(__APPLE__))
#define TYPENAME logical(4)
#else
#define TYPENAME logical(1)
#endif
#include "icdiag04.ft"
