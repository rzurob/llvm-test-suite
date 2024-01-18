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
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
character(:), allocatable  :: address1, address2
allocate (character(50) :: address1)
address1(:) = "66 Alfread Street"
if (len(address1) .ne. 50) error stop 1_4
if (address1 /= '66 Alfread Street') error stop 2_4
allocate (address2, source=address1)

address2 = "88 Lakeview Avenue"
if (len(address2) .ne. 18) error stop 3_4
if (address2 /= '88 Lakeview Avenue') error stop 4_4

deallocate(address1, address2)
end

