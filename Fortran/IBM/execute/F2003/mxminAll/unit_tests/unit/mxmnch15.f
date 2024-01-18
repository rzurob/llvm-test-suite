!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch15.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : mxmnch15
!*
!*  PROGRAMMER                 : John Zang
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Functional test
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  DESCRIPTION                : MAX/MIN - Maximum or minimum value
!*                               according to their collating sequence
!*                               of ASCII characters. 
!*                               MAXVAL/MINVAL - Maximum or minimum value
!*                               of elements in a character array.
!*                               MAXLOC/MINLOC - The location of maximum
!*                               or minimum value of elements in a character
!*                               array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer i,j,k
      integer, parameter :: aa(4,2,3) =
     + reshape((/97,98,99,100,101,102,103,104,105,106,107,108,
     +           109,110,111,112,113,114,115,116,117,118,119,120/),
     +         (/4,2,3/)) 
      integer, parameter :: bb(4,2,3) =
     + reshape((/65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     +           81,82,83,84,85,86,87,88/), (/4,2,3/)) 
      integer xx(4,2,3)
      xx = ichar(max(char(aa), char(bb))) +
     +     ichar(min(char(aa), char(bb))) - bb
      do i = 1,3
        do j = 1,2
          do k = 1,4 
            if (char(xx(k,j,i)) /= char(97 + (k - 1) + 4 * (j - 1)
     +      + 8 * (i - 1))) call zzrc((k-1)+4*(j-1)+8*(i-1)) 
          enddo
        enddo
      enddo
      end
