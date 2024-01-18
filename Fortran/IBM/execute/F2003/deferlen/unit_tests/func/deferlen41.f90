!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the MAX/MIN(LOC/VAL), TRIM
!*                               intrinsics related with characters
!*                               with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

character(:), allocatable  :: name(:)
allocate (character(8)::name(4))

!char1 = '1234  '
!char2 = ' abcd '
name(1) = "John "
name(2) = " Smith "
name(3) = "Kenneth"
name(4) = "Carol"

if (min(name(1),name(3)) .ne. "John") then
   error stop 1
end if

if (max(name(2),name(4)) .ne. "Carol") then
   error stop 2
end if

if ((minloc(name, dim=1) .ne. 2) .or. (maxloc(name, dim=1) .ne. 3)) then
   error stop 3
end if

if ((minval(name) .ne. " Smith") .or. maxval(name) .ne. ("Kenneth")) then
   error stop 4
end if

if (trim(name(1)) .ne. "John") then
   error stop 5
end if

if (trim(name(2)) .ne. " Smith") then
   error stop 6
end if

deallocate(name)

end
