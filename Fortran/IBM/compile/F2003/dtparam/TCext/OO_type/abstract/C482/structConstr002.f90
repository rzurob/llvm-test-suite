! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C482/structConstr002.f
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
! %POSTCMD: dcomp structConstr002.f
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
!*  DESCRIPTION                : Testing: The derived-type-spec shall not specify an abstract type (C401)
!*                                        Structure Constructor as Source in Allocate statement (for pointer/allocatable)           
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
   
   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type
   
end module

program structConstr002
   use m
   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   type(child(4,4)), allocatable :: c1
   type(child(4,4)), pointer     :: c2
   class(*), allocatable    :: u1
   class(*), pointer        :: u2
   
   allocate(b1,source=base(4)(4))
   allocate(b2,source=base(4)(5))
   allocate(c1,source=child(4,4)(base=base(4)(1),rid=5.6))
   allocate(c2,source=child(4,4)(base=base(4)(2),rid=7.8))
   allocate(u1,source=child(4,4)(base=base(4)(3),rid=9.1))   
   allocate(u2,source=child(4,4)(base=base(4)(4),rid=2.3))   
   
end program
