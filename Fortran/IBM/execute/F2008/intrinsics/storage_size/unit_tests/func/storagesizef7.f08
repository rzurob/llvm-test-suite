!! Test the result is consitent with the constant specified in
!! ISO_FORTRAN_ENV intrinsic module
use, intrinsic :: iso_fortran_env
integer i
real r
character c

if (storage_size(i) .ne. numeric_storage_size) then
error stop 1_4
end if

if (storage_size(r) .ne. numeric_storage_size) then
error stop 2_4
end if

if (storage_size(c) .ne. character_storage_size) then
error stop 3_4
end if
end
