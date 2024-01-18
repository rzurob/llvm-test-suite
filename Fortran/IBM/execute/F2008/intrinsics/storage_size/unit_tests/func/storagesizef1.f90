!!! Test the STORAGE_SIZE of all intrinsic types except CHARACTER as character
!!! type is testted separately.

integer result
integer i
integer(1) :: i1
integer(2) :: i2
integer(4) :: i4
integer(8) :: i8

real r
real(4) :: r4
real(8) :: r8
real(16) :: r16
real :: k(4)

complex :: cplx
complex(4) :: cplx4
complex(8) :: cplx8
complex(16) :: cplx16

logical l
logical(1) :: l1
logical(2) :: l2
logical(4) :: l4
logical(8) :: l8

byte :: b1

double precision :: dp
double complex :: dc

result = storage_size(i)
print*, result
!print*, sizeof(i), " == ", result
!if (result .ne. (sizeof(i) * 8)) then
!error stop 1_4
!end if

result = storage_size(i1)
if (result .ne. (sizeof(i1) * 8)) then
error stop 2_4
end if

result = storage_size(i2)
if (result .ne. (sizeof(i2) * 8)) then
error stop 3_4
end if

result = storage_size(i4)
if (result .ne. (sizeof(i4) * 8)) then
error stop 4_4
end if

result = storage_size(i8)
if (result .ne. (sizeof(i8) * 8)) then
error stop 5_4
end if

result = storage_size(r)
if (result .ne. (sizeof(r) * 8)) then
error stop 6_4
end if

result = storage_size(r4)
if (result .ne. (sizeof(r4) * 8)) then
error stop 7_4
end if

result = storage_size(r8)
if (result .ne. (sizeof(r8) * 8)) then
error stop 8_4
end if

result = storage_size(r16)
if (result .ne. (sizeof(r16) * 8)) then
error stop 9_4
end if

result = storage_size(cplx)
if (result .ne. (sizeof(cplx) * 8)) then
error stop 10_4
end if

result = storage_size(cplx4)
if (result .ne. (sizeof(cplx4) * 8)) then
error stop 11_4
end if

result = storage_size(cplx8)
if (result .ne. (sizeof(cplx8) * 8)) then
error stop 12_4
end if

result = storage_size(cplx16)
if (result .ne. (sizeof(cplx16) * 8)) then
error stop 13_4
end if

result = storage_size(l)
if (result .ne. (sizeof(l) * 8)) then
error stop 14_4
end if

result = storage_size(l1)
if (result .ne. (sizeof(l1) * 8)) then
error stop 15_4
end if

result = storage_size(l2)
if (result .ne. (sizeof(l2) * 8)) then
error stop 16_4
end if

result = storage_size(l4)
if (result .ne. (sizeof(l4) * 8)) then
error stop 17_4
end if

result = storage_size(l8)
if (result .ne. (sizeof(l8) * 8)) then
error stop 18_4
end if

result = storage_size(dp)
if (result .ne. (sizeof(dp) * 8)) then
error stop 19_4
end if

result = storage_size(dc)
if (result .ne. (sizeof(dc) * 8)) then
error stop 20_4
end if

result = storage_size(k)
if (result .ne. (sizeof(k(1)) * 8)) then
error stop 25_4
end if

end
