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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb containing polymorphic array of
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
         procedure, pass :: print_b_b2darray
         generic :: print => print_b_b1darray, print_b_b2darray
   end type

   type, extends (base) :: child
      integer :: i
      contains
         procedure, pass :: print_b_b1darray => print_c_c1darray
         procedure, pass :: print_c_c3darray
         generic :: print => print_c_c3darray
   end type

   contains

   subroutine print_b_b1darray ( a, b )
      class(base), intent(in) :: a, b(:)

      print *, 'print_b_b1darray:'
      print *, a%c
      print *, b%c
      select type ( b )
         type is ( child )
            print *, b%i
      end select

   end subroutine

   subroutine print_c_c1darray ( a, b )
      class(child), intent(in) :: a
      class(base), intent(in) :: b(:)

      print *, 'print_c_c1darray:'
      print *, a%c
      print *, a%i
      print *, b%c
      select type ( b )
         type is ( child )
            print *, b%i
      end select

   end subroutine

   subroutine print_b_b2darray ( a, b )
      class(base), intent(in) :: a, b(:,:)

      print *, 'print_b_b2darray:'
      print *, a%c
      select type ( a )
         type is ( child )
            print *, a%i
      end select

      print *, b%c
      select type ( b )
         type is ( child )
            print *, b%i
      end select

   end subroutine

   subroutine print_c_c3darray ( a, b )
      class(child), intent(in) :: a, b(:,:,:)

      print *, 'print_c_c3darray:'
      print *, a%c
      print *, a%i

      print *, b%c
      print *, b%i

   end subroutine

end module

program genericGenericNameArray003
   use m

   class(base), allocatable :: b0, b1(:), b2(:,:), b3(:,:,:)
   class(child), allocatable :: c0, c1(:), c2(:,:), c3(:,:,:)

   allocate ( b0, source = base('b00') )
   allocate ( b1(3), source = (/ base('b11'), base('b12'), base('b13') /)  )
   allocate ( b2(2,2), source = reshape ( source = (/ base('b21'), base('b22'), base('b23'), base('b24') /),   shape = (/2,2/) ) )
   allocate ( b3(2,2,2), source = reshape ( source = (/ child('b31',301), child('b32',302), child('b33',303), child('b34',304),   &
   &                                                    child('b35',305), child('b36',306), child('b37',307), child('b38',308) /), shape = (/2,2,2/) ) )

   allocate ( c0, source = child('c00', 1) )
   allocate ( c1(3), source = (/ child('c11',101), child('c12',102), child('c13',103) /)  )
   allocate ( c2(2,2), source = reshape ( source = (/ child('c21',201), child('c22',202), child('c23',203), child('c24',204) /),   shape = (/2,2/) ) )
   allocate ( c3(2,2,2), source = reshape ( source = (/ child('c31',301), child('c32',302), child('c33',303), child('c34',304),   &
   &                                                    child('c35',305), child('c36',306), child('c37',307), child('c38',308) /), shape = (/2,2,2/) ) )

   call b0%print(b1)
   call b0%print(b2)
   call b0%print(c1)
   call b0%print(c2)
   
   call c0%print(c1)
   call c0%print(c2)
   call c0%print(c3)

   select type ( b3 )
      class is ( child )
         call c0%print(b3)
   end select
   
   call b0%print( b1(1:3:2) )   !<- 1d array section
   call b0%print( b2(1:1,1:2) ) !<- 2d array section

   call c0%print(c1(3:1:-1))
   call c0%print(c2(2:1:-1,2:1:-1))
   call c0%print(c3(2:1:-1,2:1:-1,2:1:-1))

   select type ( b3 )
      class is ( child )
         call c0%print(b3(2:1:-1,2:1:-1,2:1:-1))
   end select
   
end program
