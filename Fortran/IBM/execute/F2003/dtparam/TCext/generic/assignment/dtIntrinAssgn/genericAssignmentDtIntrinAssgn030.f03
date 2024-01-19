! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn030.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if it's an array,
!*                                      it's allocated with the same bound.
!*                                      with elemental subroutine defined in base type
!*                                      with multi-dimensional array
!*
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

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
   end type

   type container(k3,n3)    ! (4,20)
      integer, kind                        :: k3
      integer, len                         :: n3
      class(base(:,k3)), allocatable       :: b1(:,:)
      class(child(:,k3,:,k3)), allocatable :: c1(:,:,:)
   end type

   interface assignment(=)
      module procedure arraytoarray
   end interface

   contains

      subroutine arraytoarray ( a, b )
         class(base(*,4)), intent(out) :: a(:,:)
         class(base(*,4)), intent(in)  :: b(:,:)

         stop 100

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)   :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( b )
                  type is ( child(*,4,*,4) )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn030
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container(4,20)( reshape ( source = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /), shape = (/2,2/) ), &
      &            reshape ( source = (/ child(20,4,20,4)(0,1), child(20,4,20,4)(2,3), child(20,4,20,4)(4,5), child(20,4,20,4)(6,7),  child(20,4,20,4)(8,9), child(20,4,20,4)(10,11), child(20,4,20,4)(12,13), child(20,4,20,4)(14,15) /), shape = (/2,2,2/) ) )

   print *, c1%b1%i, c1%c1%i, c1%c1%j, 'bounds:', lbound(c1%b1), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)

   c2 = c1
   c3 = c2

   print *, c2%b1%i, c2%c1%i, c2%c1%j, 'bounds:', lbound(c2%b1), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j, 'bounds:', lbound(c3%b1), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)

   deallocate ( c3%b1, c3%c1 )

   allocate ( c3%b1(4:5,8:9), source = reshape ( source = (/ child(20,4,20,4)(0,1), child(20,4,20,4)(2,3), child(20,4,20,4)(4,5), child(20,4,20,4)(6,7) /), shape = (/2,2/) ) )
   allocate ( c3%c1(-3:-2,100:101,10000:10001), source = reshape ( source = (/ child(20,4,20,4)(10,11), child(20,4,20,4)(12,13), child(20,4,20,4)(14,15), child(20,4,20,4)(16,17),  child(20,4,20,4)(18,19), child(20,4,20,4)(20,21), child(20,4,20,4)(22,23), child(20,4,20,4)(24,25)  /), shape = (/2,2,2/) ) )

   c1 = c3
   c2 = c1

   select type ( g => c1%b1 )
      type is ( child(*,4,*,4) )
         print *, g%i, g%j, c1%c1%i, c1%c1%j, 'bounds:', lbound(g), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)
   end select

   select type ( g => c2%b1 )
      type is ( child(*,4,*,4) )
         print *, g%i, g%j, c2%c1%i, c2%c1%j, 'bounds:', lbound(g), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)
   end select

   select type ( g => c3%b1 )
      type is ( child(*,4,*,4) )
         print *, g%i, g%j, c3%c1%i, c3%c1%j, 'bounds:', lbound(g), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)
   end select

end program
