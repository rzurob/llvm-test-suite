! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/array004.f
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
! %POSTCMD: dcomp array004.f
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
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        polymorphic abstract type data-ref assigned data, pointer assignment
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
   
   type :: otherbase(k3,n1)    ! (4,20)
      integer, kind                          :: k3
      integer, len                           :: n1
      class(base(k3)), dimension(:), pointer :: ptr
   end type
   
   type, extends(otherbase) :: otherchild(k4,n2)    ! (4,20,4,20)
       integer, kind :: k4
       integer, len  :: n2
   end type
   

end module

program array004
   use m  

   type(child(4,4)), target :: c1(5)
   type(otherbase(4,20)) :: ob1
   class(otherbase(4,20)), pointer :: ob2
   type(otherchild(4,20,4,20)), allocatable, target :: oc1
 
   allocate (oc1)
   ob2 => oc1
   
   ob1%ptr => c1   
   ob1%ptr => c1(1:3)%base
   ob1%ptr => c1(1:0)%base
   
   ob2%ptr  => c1   
   ob2%ptr  => c1(3:5)%base
   ob2%ptr  => c1(1:0)%base
   
   oc1%ptr  => c1   
   oc1%ptr  => c1(2:3)%base
   oc1%ptr  => c1(1:0)%base

end program
