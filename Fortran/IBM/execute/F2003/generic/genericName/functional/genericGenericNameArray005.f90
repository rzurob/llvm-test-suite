!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb containing polymorphic assumed size shape array of
!*                                             derived types dummy args of different ranks
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

   type base
      character(3) :: c
      contains
         procedure, pass :: print_b_b1darray
         procedure, pass(b) :: print_b_b2darray
         generic :: print => print_b_b1darray, print_b_b2darray
   end type

   type, extends (base) :: child
      integer :: i =-999
      contains
         procedure, pass :: print_b_b1darray => print_c_c1darray
   end type

   contains

   subroutine print_b_b1darray ( a, b )
      class(base), intent(in) :: a, b(*)

      print *, 'print_b_b1darray:'
      print *, a%c
      print *, b(1:3)%c
      select type ( b )
         type is ( child )
            print *, b(1:3)%i
      end select

   end subroutine

   subroutine print_c_c1darray ( a, b )
      class(child), intent(in) :: a
      class(base), intent(in) :: b(2:*)

      print *, 'print_c_c1darray:'
      print *, a%c
      print *, a%i
      print *, b(2:4)%c
      select type ( b )
         type is ( child )
            print *, b(2:4)%i
     end select

   end subroutine

   subroutine print_b_b2darray ( a, b )
      class(base), intent(in) :: a(2:3,1:*), b

      print *, 'print_b_b2darray:'
      print *, b%c
      select type ( b )
         type is ( child )
            print *, b%i
      end select

      print *, a(2:3,1:2)%c
      select type ( a )
         type is ( child )
            print *, a(2:3,1:2)%i
      end select

   end subroutine

end module

program genericGenericNameArray005
   use m

   class(base), allocatable :: b0, b1(:), b2(:,:)
   class(child), allocatable :: c0, c1(:), c2(:,:)

   allocate ( b0, source = base('b00') )
   allocate ( b1(5), source = (/ base('b11'), base('b12'), base('b13'), base('b14'),base('b15') /)  )
   allocate ( b2(3,3), source = reshape ( source = (/ base('b21'), base('b22'), base('b23') &
   &                                                , base('b24'), base('b25'), base('b26') &
   &                                                , base('b27'), base('b28'), base('b29')  /),   shape = (/3,3/) ) )

   allocate ( c0, source = child('c00', 1) )
   allocate ( c1(5), source = (/ child('c11',101), child('c12',102), child('c13',103), child('c14',104), child('c15',105) /)  )
   allocate ( c2(3,3), source = reshape ( source = (/ child('c21',201), child('c22',202), child('c23',203)  &
   &                                                , child('c24',204), child('c25',205), child('c26',206)  &
   &                                                , child('c27',207), child('c28',208), child('c29',209) /),   shape = (/3,3/) ) )


   ! pass in the whole array

   print *,'whole array:'
   call b0%print(b1)
   call b0%print(b2)
   call b0%print(c1)
   call b0%print(c2)

   call c0%print(b1)
   call c0%print(b2)
   call c0%print(c1)
   call c0%print(c2)

   ! array section

   print *,'array section:'
   call b0%print(b1(3:))
   call b0%print(c2(2:,1:))

   call c0%print(c1(2:4))
   call c0%print(b2(1:2,2:1:-1))

   ! array constructor

   call b0%print((/base('1xx'), base('2xx'), base('3xx')/))
   call b0%print((/child('1xx'), child('2xx'), child('3xx')/))
   call b0%print(reshape ( source = (/base('1xx'), base('2xx'), base('3xx'), base('4xx')/), shape = (/2,2/) ))

   call c0%print((/base('abc'), base('def'), base('ghi')/))
   call c0%print((/child('jkl',100), child('mno'), child('pqr',300)/))
   call c0%print(reshape ( source = (/child('x1x'), child('x2x'), child('x3x'), child('x4x')/), shape = (/2,2/) ))

end program
