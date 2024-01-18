!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg09.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-08-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Enhancement to determining dummy argument presence
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 386700
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Arguments using copy-in/out
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test1(a, b)
      character(*), intent(in) :: a
      character, optional :: b(:)
      call test_1(a, b)
   end subroutine test1

   subroutine test_1(a, b)
      character(*), intent(in) :: a
      character, optional :: b(:)
      print *,a
      print *,present(b)
   end subroutine test_1

   subroutine test1_1(a, b)
      character(*), intent(in) :: a
      character, optional :: b(:)
      call test_1_1(a, b)
   end subroutine test1_1

   subroutine test_1_1(a, b)
      character(*), intent(in) :: a
      character, optional :: b(:)
      print *,a
      print *,present(b)
      if (present(b)) then
         print *,b
      end if
   end subroutine test_1_1

   subroutine test2(a, b)
      character(*), intent(in) :: a
      integer, optional :: b(15)
      call test_2(a, b)
   end subroutine test2

   subroutine test_2(a, b)
      character(*), intent(in) :: a
      integer, optional :: b(15)
      print *,a
      print *,present(b)
   end subroutine test_2

   subroutine test2_1(a, b)
      character(*), intent(in) :: a
      integer, optional :: b(5)
      call test_2_1(a, b)
   end subroutine test2_1

   subroutine test_2_1(a, b)
      character(*), intent(in) :: a
      integer, optional :: b(5)
      print *,a
      print *,present(b)
      if (present(b)) then
         print *,b
      end if
   end subroutine test_2_1

   subroutine test3(a, b, c)
      character(*), intent(in) :: a
      character, optional :: b(:)
      integer, optional :: c(15)
      call test_3(a, b, c)
   end subroutine test3

   subroutine test_3(a, b, c)
      character(*), intent(in) :: a
      character, optional :: b(:)
      integer, optional :: c(15)
      print *,a
      print *,present(b)
      print *,present(c)
   end subroutine test_3

   subroutine test3_1(a, b, c)
      character(*), intent(in) :: a
      character, optional :: b(:)
      integer, optional :: c(7)
      call test_3_1(a, b, c)
   end subroutine test3_1

   subroutine test_3_1(a, b, c)
      character(*), intent(in) :: a
      character, optional :: b(:)
      integer, optional :: c(7)
      print *,a
      print *,present(b)
      print *,present(c)
      if (present(b)) then
         print *,b
      end if
      if (present(c)) then
         print *,c
      end if
   end subroutine test_3_1

end module testM

program main

   use testM
   implicit none
   character, pointer :: string(:)
   integer, pointer :: array(:)

   nullify (string)
   call test1 ("Hello World Copy Character")
   call test1 ("Hello World Copy Character", string)
   allocate(string(5))
   string = (/ 'd','u','m','m','y' /)
   call test1_1 ("Hello World Copy Character", string)
   deallocate(string)

   nullify (array)
   call test2 ("Hello World Copy Integer")
   call test2 ("Hello World Copy Integer", array)
   allocate(array(5))
   array = (/ 5,0,0,0,5 /)
   call test2_1 ("Hello World Copy Integer", array)
   deallocate(array)

   nullify (string)
   nullify (array)
   call test3 ("Hello World Copy Character Integer", string, array)
   allocate(string(7))
   allocate(array(7))
   string = (/ 'd','u','m','m','y',' ','1' /)
   array = (/ 7,0,0,0,0,0,7 /)
   call test3_1 ("Hello World Copy Character Integer", string, array)
   deallocate(array)
   deallocate(string)

end program main
