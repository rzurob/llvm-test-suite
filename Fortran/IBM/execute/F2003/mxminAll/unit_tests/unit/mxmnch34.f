!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch34.f 
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
!*  TEST CASE TITLE            : mxmnch34
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
      character(10), parameter :: aa(9) = 
     + (/'a','b','c','i','e','f','g','h','d'/)
      character(10) bb(100)
      integer xx(1)

      bb = '!'
      bb(3) = 'ibm'
      bb(6) = 'usa'
      bb(12) = 'can'
      bb(40) = 'bel'
      bb(8) = 'jan'
      bb(23) = 'gb'
      
      if (maxval(bb((/4,6,40,23/))) /= 'usa       ') error stop 1

      xx = maxloc(bb((/4,6,40,23/)))
      if (xx(1) /= 2) error stop 2

      if (maxval(aa((/3,4,5,7/))) /= 'i         ') error stop 3

      xx = maxloc(aa((/3,4,5,7/)))
      if (xx(1) /= 2) error stop 4

      end
