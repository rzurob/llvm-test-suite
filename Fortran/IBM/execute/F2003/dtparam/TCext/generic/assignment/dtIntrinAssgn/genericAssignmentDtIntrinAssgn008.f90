! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn008.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - A derived-type intrinsic assignment is performed as if each component of variable
!*                                    were assigned from the corresponding component of expr using pointer
!*                                    assignment for each pointer component
!*                                      - in the same derived type as component, pointer, allocatable in a containing
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x
      contains
         procedure, pass :: baseassignment
         generic :: assignment(=) => baseassignment
   end type


   type container(k2)    ! (4)
      integer, kind               :: k2
      type(base(k2))              :: b1
      type(base(k2)), pointer     :: b2
      type(base(k2)), allocatable :: b3
   end type

   type, extends(container) :: child_container    ! (4)
      type(base(k2)) :: bb1
      type(base(k2)), pointer :: bb2
      type(base(k2)), allocatable :: bb3
   end type

   contains

      subroutine baseassignment ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b

         a%x = b%x + 1

         print *, 'baseassignment'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn008
   use m

   type(container(4)) :: c1
   type(child_container(4)), pointer :: c2
   class(container(4)), allocatable :: c3

   type(base(4)), pointer :: b1
   allocate ( b1,source = base(4)(200) )

   print *, 'set c1'
   c1 = container(4)( base(4)(100), b1, base(4)(300) )
   print *, 'allocate c2'
   allocate ( c2 )
   c2 = child_container(4)( base(4)(1000), b1, base(4)(2000), base(4)(3000), b1, base(4)(4000) )
   print *, 'allocate c3'
   allocate ( c3, source = container(4)( base(4)(-999), null(), base(4)(-999) ) )

   select type ( c3 )
      type is ( container(4) )
         print *, 'set c3'
         c3 = c1
         print *, c3%b1, c3%b2, c3%b3, associated ( c3%b2, c1%b2 ), associated ( c3%b2, b1 )
   end select

   deallocate ( c3 )
   allocate ( child_container(4) :: c3 )

   select type ( c3 )
      type is ( child_container(4) )
         print *, 'set c3'
         c3 = c2
         print *, c3%b1, c3%b2, c3%b3, c3%bb1, c3%bb2, c3%bb3, associated ( c3%b2, c2%b2 ), associated ( c3%b2, b1 )
   end select

end program
