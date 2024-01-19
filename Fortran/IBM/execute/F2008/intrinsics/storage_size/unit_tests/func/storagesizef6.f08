!!! Test vector types

integer result

vector(integer) :: vint
vector(real) :: vreal
vector(pixel) :: vpixel
vector(unsigned) :: vunsigned

result = storage_size(vint)
if (result .ne. (sizeof(vint) * 8)) then
error stop 1_4
end if

result = storage_size(vreal)
if (result .ne. (sizeof(vreal) * 8)) then
error stop 2_4
end if

result = storage_size(vpixel)
if (result .ne. (sizeof(vpixel) * 8)) then
error stop 3_4
end if

result = storage_size(vunsigned)
if (result .ne. (sizeof(vunsigned) * 8)) then
error stop 4_4
end if

end
