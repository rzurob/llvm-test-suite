!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the concatanation operator (//)
!*                               for characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character(:), allocatable  :: char1, char2
character(4) char3
character(10) result
allocate (character(6)::char1)
allocate (char2, source=char3)
char1 = '123456'
char2 = '7890'

result = char1//'ABCD'
if (result /= '123456ABCD') error stop 1

result = 'AB'//char1//'CD'
if (result /= 'AB123456CD') error stop 2

result = char1//char2
if (result /= '1234567890') error stop 3

result = char1(1:4)//char2//char1(5:6)
if (result /= '1234789056') error stop 4

deallocate (char1, char2)
end

