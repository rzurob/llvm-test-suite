! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn003.f
! with manual adjustment for specification expression (a%n1)
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, class(abstract type)
!*                                        returns extension of abstract type
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

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type


contains

   function foo(a)
      type(child(4,4,*)), intent(in) :: a
      type(child(4,4,a%n1)), pointer :: foo

      allocate(foo, source=a )
   end function

   function foo1(a) result (boo)
      type(child(4,4,*)), intent(in) :: a
      class(child(4,4,a%n1)), pointer :: boo
      allocate(boo, source=a )
   end function

end module

program funcRetrn003
   use m

   class(base(4)), allocatable :: c
   class(child(4,4,20)), allocatable :: c1

   allocate (c1, source = child(4,4,20)(4))

   allocate ( c,source=foo(c1) )
   if (c%id .ne. 4) error stop 1_4

   deallocate (c)
   allocate ( c, source=foo1(child(4,4,20)(5)) )

   if (c%id .ne. 5) error stop 2_4

end program

