! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn027.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*                                 - for allocatable component
!*                                    - if it's an array,
!*                                      it's allocated with the same bound.
!*                                      with elemental subroutine defined in base type
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

   type container(k2,n2)    ! (4,20)
      integer, kind                   :: k2
      integer, len                    :: n2
      class(base(:,k2)), allocatable   :: b1(:)
      type(child(:,k2)), allocatable   :: c1(:)
   end type

   interface assignment(=)
      module procedure bassgn
   end interface

   contains

      elemental subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b

         a%i = b%i + 1
         select type ( a )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     a%j = b%j + 1
               end select
         end select

      end subroutine


end module

program genericAssignmentDtIntrinAssgn027
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container(4,20)( (/ base(20,4)(1), base(20,4)(2), base(20,4)(3) /), (/ child(20,4)(4,5), child(20,4)(6,7), child(20,4)(8,9) /) )
   print *, c1%b1%i, c1%c1%i, c1%c1%j, 'bounds', lbound(c1%b1), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)

   c2 = c1
   print *, c2%b1%i, c2%c1%i, c2%c1%j, 'bounds', lbound(c2%b1), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)

   c3 = c2
   print *, c3%b1%i, c3%c1%i, c3%c1%j, 'bounds', lbound(c3%b1), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)

   deallocate ( c2%b1, c2%c1 )
   allocate ( c2%b1(2:5), source = (/ child(20,4)(1,2), child(20,4)(3,4), child(20,4)(5,6), child(20,4)(7,8) /) )
   allocate ( c2%c1(9:11), source = (/ child(20,4)(11,12), child(20,4)(13,14), child(20,4)(15,16)/) )

   c3 = c2
   c1 = c3

   select type ( g => c1%b1 )
      type is ( child(*,4) )
         print *, g, c1%c1, 'bounds:', lbound(g), ubound(g), lbound(c1%c1), ubound(c1%c1)
   end select

   select type ( g => c2%b1 )
      type is ( child(*,4) )
         print *, g, c2%c1, 'bounds:', lbound(g), ubound(g), lbound(c2%c1), ubound(c2%c1)
   end select

   select type ( g => c3%b1 )
      type is ( child(*,4) )
         print *, g, c3%c1, 'bounds:', lbound(g), ubound(g), lbound(c3%c1), ubound(c3%c1)
   end select

   deallocate ( c3%b1, c3%c1 )
   allocate ( c3%b1(20:29)  , source = (/ ( child(20,4)(j,j+1), j = 12,30,2 ) /) )
   select type ( g => c3%b1(20:29) )
      type is ( child(*,4) )
         allocate ( c3%c1(-29:-20), source = g )
   end select

   c2 = c3
   c1 = c2

   select type ( g => c1%b1 )
      type is ( child(*,4) )
         print *, g, c1%c1, 'bounds:', lbound(g), ubound(g), lbound(c1%c1), ubound(c1%c1)
   end select

   select type ( g => c2%b1 )
      type is ( child(*,4) )
         print *, g, c2%c1, 'bounds:', lbound(g), ubound(g), lbound(c2%c1), ubound(c2%c1)
   end select

   select type ( g => c3%b1 )
      type is ( child(*,4) )
         print *, g, c3%c1, 'bounds:', lbound(g), ubound(g), lbound(c3%c1), ubound(c3%c1)
   end select

end program
