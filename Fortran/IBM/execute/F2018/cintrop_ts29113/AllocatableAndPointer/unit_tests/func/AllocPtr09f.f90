!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr09f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Scalarization and elemental calls
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(ptr, al) bind(c)
          import
          real(c_float), pointer :: ptr(:,:)
          real(c_float), allocatable, target :: al(:,:)
        end
      end interface

      real(c_float), pointer :: ptr(:,:)
      real(c_float), allocatable, target :: al(:,:)

      ptr => NULL()

      call test(ptr, al)

      end

      subroutine test(a, b) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        interface
           elemental function foo(a)
             import
             integer(c_int) :: foo
             real(c_float), intent(in) :: a
           end
        end interface

        ! Arguments
        real(c_float), pointer :: a(:, :)
        real(c_float), allocatable, target :: b(:, :)

        ! Locals
        integer :: i, j
        real(c_float), parameter :: pi = 3.14159265
        real(c_float), target :: t1(3,2)
        real(c_float) :: local_array(5,5) = 0.0

        t1 = reshape([(2*pi/i, i=1,6,1)], [3,2])

        ! ****** Test 1: ****** !
        print *, "Test 1:"
        a => t1
        print '(6F6.2)', cos(a)
        print *, maxval(cos(a))
        print *, minval(cos(a))

        ! ****** Test 2: ****** !
        print *, "Test 2:"
        allocate(b(3,2), source=t1)
        print '(6F6.2)', cos(b)
        print *, maxval(cos(b))
        print *, minval(cos(b))
        deallocate(b)

        ! ****** Test 3: ****** !
        print *, "Test 3:"
        allocate(a(10,5))
        a = reshape([(i, i=1,50,1)], [10,5])
        allocate(b(10,5))
        b = a
        if(any(a .ne. b)) error stop 1
        print '(50F5.1)', a
        print '(50F5.1)', b
        deallocate(a)
        deallocate(b)

        ! ****** Test 4: ****** !
        print *, "Test 4:"
        allocate(a(3,2), b(3,2))
        a = t1
        b = -t1
        if(any(t1 .ne. a) .or. any(-t1 .ne. b)) error stop 2
        print '(6F5.1)', a
        print '(6F5.1)', b
        deallocate(a, b)

        ! ****** Test 5: ****** !
        print *, "Test 5:"
        j = 1
        allocate(b(5,5))
        b = reshape([(i, i=1,j*25,i)], [j*5,j*5])
        a => b
        local_array = b
        if(any(local_array .ne. a)) error stop 3
        print '(25F5.1)', a
        print '(25F5.1)', b
        print '(25F5.1)', local_array
        deallocate(b)

        
        ! ****** Test 6: ****** !
        print *, "Test 6:"
        allocate(a(5,5), b(5,5))
        a = -local_array
        b = foo(a)
        print '(25F6.1)', b
        print *, foo(a)
        local_array = -b
        print '(25F6.1)', local_array
        b = local_array
        print *, foo(b)
        deallocate(a, b)

        ! ****** Test 7: ****** !
        print *, "Test 7:"
        allocate(a(3,2), b(2,3))
        j = 6
        a = j
        b = -j
        a(3,2) = -j
        b(2,3) = j
        print '(6F6.2)', a
        print '(6F6.2)', b
        deallocate(a, b)
         


    end subroutine

    elemental function foo(a)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: foo
      real(c_float), intent(in) :: a
      foo = int(a)
    end
