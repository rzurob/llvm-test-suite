!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullDiagGeneric02.f
!*
!*  DATE                       : Sept. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. ACTUAL ARGUMENTS TO GENERIC PROCEDURE ARE DIFFERENT TYPE , TYPE PARAMETER AND RANK
!* 3. ACTUAL ARGUMENT IS NULL(),MOLD IS ABSENT,SHOULD HAVE DIAGNOSTIC MESSAGE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type typeA(k)
      integer,kind :: k=4
   end type
   type typeB(k)
      integer,kind :: k=4
   end type
   type any
      contains
         procedure,nopass :: proc1=>sub1
         procedure,nopass :: proc2=>sub2
         procedure,nopass :: proc3=>sub3
         procedure,nopass :: proc4=>sub4
         procedure,nopass :: proc5=>sub5
         procedure,nopass :: proc6=>sub6
         generic :: proc=>proc1,proc2,proc3,proc4,proc5,proc6
   end type

   interface
     subroutine sub1(dt)
       import
       type(typeA(4)),pointer :: dt
     end subroutine
     subroutine sub2(dt)
       import
       type(typeA(2)),allocatable :: dt
     end subroutine
     subroutine sub3(dt)
       import
       type(typeB(4)),pointer  :: dt
     end subroutine
     subroutine sub4(dt)
       import
       type(typeB(2)),allocatable :: dt
     end subroutine
     subroutine sub5(dt)
       import
       type(typeB(4)),pointer  :: dt(:)
     end subroutine
     subroutine sub6(dt)
       import
       type(typeB(4)),pointer  :: dt(:,:)
     end subroutine
   end interface

end module

program nullDiagGeneric02
   use m
   implicit none

   type(typeA(4)),pointer     :: a1
   type(typeA(2)),allocatable :: a2
   type(typeB(4)),pointer     :: b1
   type(typeB(2)),allocatable :: b2
   type(typeB(4)),pointer     :: b3(:)
   type(typeB(4)),pointer     :: b4(:,:)

   type(any) :: any1

   call any1%proc(null())
   call any1%proc(null())
   call any1%proc(null())
   call any1%proc(null())
   call any1%proc(null())
   call any1%proc(null())

end program

      subroutine sub1(dt)
         use m,only : typeA
         type(typeA(4)),pointer :: dt
      end subroutine

      subroutine sub2(dt)
         use m,only : typeA
         type(typeA(2)),allocatable :: dt
      end subroutine

      subroutine sub3(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt
      end subroutine

      subroutine sub4(dt)
         use m,only : typeB
         type(typeB(2)),allocatable :: dt
      end subroutine

      subroutine sub5(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:)
      end subroutine

      subroutine sub6(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:,:)
      end subroutine
