! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentUnlimitedPoly001.f
! opt variations: -qnok -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: 2nd operand is unlimited polymorphic
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
   type newtype(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type :: base(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
      class(*), allocatable :: i
      contains
         procedure, pass :: bagnmt
         generic :: assignment(=) => bagnmt
   end type

   contains

      subroutine bagnmt ( a, b )
         class(base(4,*)), intent(out) :: a
         class(*), intent(in) :: b

         if ( allocated(a%i) ) deallocate ( a%i )
         allocate ( a%i , source = b )

         print *, 'basgmt'

      end subroutine

end module

program genericAssignmentUnlimitedPoly001
   use m

   class(base(4,:)), allocatable :: b1
   type (base(4,20)) :: b2

   allocate ( b1, source = base(4,20) ( 'ibm' ) )
   b2 = b1

   select type ( g => b2%i )
      type is ( base(4,*) )
         select type ( j => g%i )
            type is ( character(*) )
               print *, j
         end select
   end select

   b2 = 123

   select type ( g => b2%i )
      type is ( integer )
         print *, g
   end select

   b1 = .true.

   select type ( g => b1%i )
      type is ( logical )
         print *, g
   end select

   b2 = 1.001

   select type ( g => b2%i )
      type is ( real )
         write(6,'(f6.3)')  g
   end select

   b1 = newtype(20,4)(1000)
   select type ( g => b1%i )
      type is ( newtype(*,4) )
         print *, g
   end select

end program
