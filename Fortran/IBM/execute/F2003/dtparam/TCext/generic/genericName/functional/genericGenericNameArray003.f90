! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameArray003.f
! opt variations: -qnock -qnodeferredlp

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

   type base(k1,n1)    ! (1,3)
      integer, kind             :: k1
      integer, len              :: n1
      character(kind=k1,len=n1) :: c
      contains
         procedure, pass :: print_b_b1darray
         procedure, pass :: print_b_b2darray
         generic :: print => print_b_b1darray, print_b_b2darray
   end type

   type, extends (base) :: child(k2)    ! (1,3,4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: print_b_b1darray => print_c_c1darray
         procedure, pass :: print_c_c3darray
         generic :: print => print_c_c3darray
   end type

   contains

   subroutine print_b_b1darray ( a, b )
      class(base(1,*)), intent(in) :: a, b(:)

      print *, 'print_b_b1darray:'
      print *, a%c
      print *, b%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b%i
      end select

   end subroutine

   subroutine print_c_c1darray ( a, b )
      class(child(1,*,4)), intent(in) :: a
      class(base(1,*)), intent(in) :: b(:)

      print *, 'print_c_c1darray:'
      print *, a%c
      print *, a%i
      print *, b%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b%i
      end select

   end subroutine

   subroutine print_b_b2darray ( a, b )
      class(base(1,*)), intent(in) :: a, b(:,:)

      print *, 'print_b_b2darray:'
      print *, a%c
      select type ( a )
         type is ( child(1,*,4) )
            print *, a%i
      end select

      print *, b%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b%i
      end select

   end subroutine

   subroutine print_c_c3darray ( a, b )
      class(child(1,*,4)), intent(in) :: a, b(:,:,:)

      print *, 'print_c_c3darray:'
      print *, a%c
      print *, a%i

      print *, b%c
      print *, b%i

   end subroutine

end module

program genericGenericNameArray003
   use m

   class(base(1,:)), allocatable :: b0, b1(:), b2(:,:), b3(:,:,:)
   class(child(1,:,4)), allocatable :: c0, c1(:), c2(:,:), c3(:,:,:)

   allocate ( b0, source = base(1,3)('b00') )
   allocate ( b1(3), source = (/ base(1,3)('b11'), base(1,3)('b12'), base(1,3)('b13') /)  )
   allocate ( b2(2,2), source = reshape ( source = (/ base(1,3)('b21'), base(1,3)('b22'), base(1,3)('b23'), base(1,3)('b24') /),   shape = (/2,2/) ) )
   allocate ( b3(2,2,2), source = reshape ( source = (/ child(1,3,4)('b31',301), child(1,3,4)('b32',302), child(1,3,4)('b33',303), child(1,3,4)('b34',304),   &
   &                                                    child(1,3,4)('b35',305), child(1,3,4)('b36',306), child(1,3,4)('b37',307), child(1,3,4)('b38',308) /), shape = (/2,2,2/) ) )

   allocate ( c0, source = child(1,3,4)('c00', 1) )
   allocate ( c1(3), source = (/ child(1,3,4)('c11',101), child(1,3,4)('c12',102), child(1,3,4)('c13',103) /)  )
   allocate ( c2(2,2), source = reshape ( source = (/ child(1,3,4)('c21',201), child(1,3,4)('c22',202), child(1,3,4)('c23',203), child(1,3,4)('c24',204) /),   shape = (/2,2/) ) )
   allocate ( c3(2,2,2), source = reshape ( source = (/ child(1,3,4)('c31',301), child(1,3,4)('c32',302), child(1,3,4)('c33',303), child(1,3,4)('c34',304),   &
   &                                                    child(1,3,4)('c35',305), child(1,3,4)('c36',306), child(1,3,4)('c37',307), child(1,3,4)('c38',308) /), shape = (/2,2,2/) ) )

   call b0%print(b1)
   call b0%print(b2)
   call b0%print(c1)
   call b0%print(c2)
   
   call c0%print(c1)
   call c0%print(c2)
   call c0%print(c3)

   select type ( b3 )
      class is ( child(1,*,4) )
         call c0%print(b3)
   end select
   
   call b0%print( b1(1:3:2) )   !<- 1d array section
   call b0%print( b2(1:1,1:2) ) !<- 2d array section

   call c0%print(c1(3:1:-1))
   call c0%print(c2(2:1:-1,2:1:-1))
   call c0%print(c3(2:1:-1,2:1:-1,2:1:-1))

   select type ( b3 )
      class is ( child(1,*,4) )
         call c0%print(b3(2:1:-1,2:1:-1,2:1:-1))
   end select
   
end program
