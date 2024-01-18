! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/proceRef001.f
! SCCS ID Information
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
! %POSTCMD: dcomp proceRef001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic non-rightmost scalar object call type bound procedures
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
   contains
      procedure, nopass :: printtype => printbase
      procedure, pass :: getid => getbaseid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: rid
   contains
      procedure, nopass :: printtype => printchild
      procedure, pass :: getid => getchildid
   end type

contains

   subroutine printchild()
      print *,'child'
   end subroutine

   subroutine printbase()
      print *,'base'
   end subroutine

   integer function getbaseid(a)
      class(base(4)), intent(in) :: a
      getbaseid = a%id
   end function

   integer function getchildid(a)
      class(child(4,4)), intent(in) :: a
      getchildid = a%rid
   end function

end module

program proceRef001
   use m

   type(child(4,4)) :: c1 = child(4,4) (1,2.3)
   class(child(4,4)), allocatable :: c2
   class(child(4,4)), pointer :: c3

   allocate(c2, source= child(4,4)(1,2.3))
   allocate(c3, source= child(4,4)(9,10.3))

   call c1%base%printtype()   !<- illegal
   call c2%base%printtype()   !<- illegal
   call c3%base%printtype()   !<- illegal

   if ( c1%base%getid() .eq. 1 )      error stop 1_4   !<- illegal
   if ( c2%base%getid() .eq. 1 )      error stop 2_4   !<- illegal
   if ( c3%base%getid() .eq. 9 )      error stop 3_4   !<- illegal

end program
