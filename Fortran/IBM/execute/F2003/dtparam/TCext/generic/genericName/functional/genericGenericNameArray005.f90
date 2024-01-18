! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameArray005.f
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

   type base(k1,n1)    ! (1,3)
      integer, kind             :: k1
      integer, len              :: n1
      character(kind=k1,len=n1) :: c
      contains
         procedure, pass :: print_b_b1darray
         procedure, pass(b) :: print_b_b2darray
         generic :: print => print_b_b1darray, print_b_b2darray
   end type

   type, extends (base) :: child(k2)    ! (1,3,4)
      integer, kind :: k2
      integer(k2)   :: i =-999
      contains
         procedure, pass :: print_b_b1darray => print_c_c1darray
   end type

   contains

   subroutine print_b_b1darray ( a, b )
      class(base(1,*)), intent(in) :: a, b(*)

      print *, 'print_b_b1darray:'
      print *, a%c
      print *, b(1:3)%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b(1:3)%i
      end select

   end subroutine

   subroutine print_c_c1darray ( a, b )
      class(child(1,*,4)), intent(in) :: a
      class(base(1,*)), intent(in) :: b(2:*)

      print *, 'print_c_c1darray:'
      print *, a%c
      print *, a%i
      print *, b(2:4)%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b(2:4)%i
     end select

   end subroutine

   subroutine print_b_b2darray ( a, b )
      class(base(1,*)), intent(in) :: a(2:3,1:*), b

      print *, 'print_b_b2darray:'
      print *, b%c
      select type ( b )
         type is ( child(1,*,4) )
            print *, b%i
      end select

      print *, a(2:3,1:2)%c
      select type ( a )
         type is ( child(1,*,4) )
            print *, a(2:3,1:2)%i
      end select

   end subroutine

end module

program genericGenericNameArray005
   use m

   class(base(1,:)), allocatable :: b0, b1(:), b2(:,:)
   class(child(1,:,4)), allocatable :: c0, c1(:), c2(:,:)

   allocate ( b0, source = base(1,3)('b00') )
   allocate ( b1(5), source = (/ base(1,3)('b11'), base(1,3)('b12'), base(1,3)('b13'), base(1,3)('b14'),base(1,3)('b15') /)  )
   allocate ( b2(3,3), source = reshape ( source = (/ base(1,3)('b21'), base(1,3)('b22'), base(1,3)('b23') &
   &                                                , base(1,3)('b24'), base(1,3)('b25'), base(1,3)('b26') &
   &                                                , base(1,3)('b27'), base(1,3)('b28'), base(1,3)('b29')  /),   shape = (/3,3/) ) )

   allocate ( c0, source = child(1,3,4)('c00', 1) )
   allocate ( c1(5), source = (/ child(1,3,4)('c11',101), child(1,3,4)('c12',102), child(1,3,4)('c13',103), child(1,3,4)('c14',104), child(1,3,4)('c15',105) /)  )
   allocate ( c2(3,3), source = reshape ( source = (/ child(1,3,4)('c21',201), child(1,3,4)('c22',202), child(1,3,4)('c23',203)  &
   &                                                , child(1,3,4)('c24',204), child(1,3,4)('c25',205), child(1,3,4)('c26',206)  &
   &                                                , child(1,3,4)('c27',207), child(1,3,4)('c28',208), child(1,3,4)('c29',209) /),   shape = (/3,3/) ) )


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

   call b0%print((/base(1,3)('1xx'), base(1,3)('2xx'), base(1,3)('3xx')/))
   call b0%print((/child(1,3,4)('1xx'), child(1,3,4)('2xx'), child(1,3,4)('3xx')/))
   call b0%print(reshape ( source = (/base(1,3)('1xx'), base(1,3)('2xx'), base(1,3)('3xx'), base(1,3)('4xx')/), shape = (/2,2/) ))

   call c0%print((/base(1,3)('abc'), base(1,3)('def'), base(1,3)('ghi')/))
   call c0%print((/child(1,3,4)('jkl',100), child(1,3,4)('mno'), child(1,3,4)('pqr',300)/))
   call c0%print(reshape ( source = (/child(1,3,4)('x1x'), child(1,3,4)('x2x'), child(1,3,4)('x3x'), child(1,3,4)('x4x')/), shape = (/2,2/) ))

end program
