! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPure002.f
! opt variations: -qnol -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: pure subroutine with arrays and class hierarchy
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
      integer(k1)   :: i = -999
      contains
         procedure, pass :: assgn  => bassgn
         procedure, pass :: assgn1d => bassgn1d
         generic :: assignment(=) => assgn, assgn1d
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass :: assgn  => cassgn
         procedure, pass :: assgn1d => cassgn1d
   end type

   contains

      pure subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i

      end subroutine

      pure subroutine bassgn1d ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         do i = 1, size(b)
            a = b(i)        !<- assigned the last item
         end do

      end subroutine

      pure subroutine cassgn ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         select type ( b )
            type is ( base(*,4) )
               a%i = b%i
            type is ( child(*,4) )
               a%base = b%base
               a%j = b%j
         end select

      end subroutine

      pure subroutine cassgn1d ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         do i = 1, size(b)
            a = b(i)        !<- assigned the last item
         end do

      end subroutine

end module

program genericAssignmentPure002
   use m

   class(base(20,4)), allocatable  :: b2(:)
   class(child(20,4)), allocatable :: c2(:)

   allocate ( c2(100), source = (/ ( child(20,4) ( i*500, i*1000 ),i=1,100 ) /) )

   allocate ( b2(50) )

   b2(50) = c2
   print *, b2(50)%i

   b2(49) = c2(1:99)
   print*, b2(49)%i

   c2(10) = c2(5:15)
   print *, c2(10)%i, c2(10)%j

   c2(1) = b2( (/ 1,1,1,1,1,1,1,1,50 /) )
   print *, c2(1)%i, c2(1)%j

   b2(50) = b2(49)
   print *, b2(50)%i

   deallocate ( b2 )
   allocate ( child(20,4) :: b2(100) )

   do i = 1, 100
      b2(i) = c2
   end do

   select type ( b2 )
      type is ( child(*,4) )
         do i = 1, 100
            if ( ( b2(i)%i  /= c2(100)%i ) .or. ( b2(i)%j  /= c2(100)%j ) ) error stop 1_4
         end do
   end select

end program
