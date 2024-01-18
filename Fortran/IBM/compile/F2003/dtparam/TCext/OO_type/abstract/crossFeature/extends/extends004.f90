! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends004.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp extends004.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent component has private accessibility
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: super(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
   end type
   
   type,  abstract, private, extends(super) :: base(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
   contains
      procedure, nopass :: print => printbase
   end type
   
   type, extends(base) :: child(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: r
   end type
   
   class(child(4,4,4)), allocatable :: c1
   
contains
   subroutine printbase()
      print *, "base"
   end subroutine
end module


program extends004
   use m
   
   call c1%print()
   allocate(c1, source = child(4,4,4)(1,2,3) )
   
   print *, c1%super%i
   print *, c1%i, c1%j, c1%r
   
   print *, c1%base%i
   print *, c1%base%j   

end program   
