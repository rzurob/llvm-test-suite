
     interface Array_Check

!**********************************************************
!*       One dimensional arrays                           *
!**********************************************************
        function Array_Check_I2 (array1, array2) result (res)
           integer*2 array1(:), array2(:)
           logical res
        end function Array_Check_I2

        function Array_Check_I4 (array1, array2) result (res)
           integer*4 array1(:), array2(:)
           logical res
        end function Array_Check_I4

        function Array_Check_I8 (array1, array2) result (res)
           integer*8 array1(:), array2(:)
           logical res
        end function Array_Check_I8

        function Array_Check_R4 (array1, array2) result (res)
           real*4  array1(:), array2(:)
           logical res
        end function Array_Check_R4

        function Array_Check_R8 (array1, array2) result (res)
           real*8  array1(:), array2(:)
           logical res
        end function Array_Check_R8

        function Array_Check_R16 (array1, array2) result (res)
           real*16 array1(:), array2(:)
           logical res
        end function Array_Check_R16

        function Array_Check_X8 (array1, array2) result (res)
           complex*8 array1(:), array2(:)
           logical res
        end function Array_Check_X8

        function Array_Check_X16 (array1, array2) result (res)
           complex*16 array1(:), array2(:)
           logical res
        end function Array_Check_X16

        function Array_Check_X32 (array1, array2) result (res)
           complex*32 array1(:), array2(:)
           logical res
        end function Array_Check_X32

        function Array_Check_L2 (array1, array2) result (res)
           logical*2 array1(:), array2(:)
           logical res
        end function Array_Check_L2

        function Array_Check_L4 (array1, array2) result (res)
           logical*4 array1(:), array2(:)
           logical res
        end function Array_Check_L4

        function Array_Check_L8 (array1, array2) result (res)
           logical*8 array1(:), array2(:)
           logical res
        end function Array_Check_L8

        function Array_Check_C (array1, array2) result (res)
           character*(*) array1(:), array2(:)
           logical res
        end function Array_Check_C


!**********************************************************
!*       Two dimensional arrays                           *
!**********************************************************

        function Array_Check_I2_2 (array1, array2) result (res)
           integer*2 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_I2_2

        function Array_Check_I4_2 (array1, array2) result (res)
           integer*4 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_I4_2

        function Array_Check_I8_2 (array1, array2) result (res)
           integer*8 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_I8_2

        function Array_Check_R4_2 (array1, array2) result (res)
           real*4 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_R4_2

        function Array_Check_R8_2 (array1, array2) result (res)
           real*8 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_R8_2

        function Array_Check_R16_2 (array1, array2) result (res)
           real*16 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_R16_2

        function Array_Check_X8_2 (array1, array2) result (res)
           complex*8 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_X8_2

        function Array_Check_X16_2 (array1, array2) result (res)
           complex*16 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_X16_2

        function Array_Check_X32_2 (array1, array2) result (res)
           complex*32 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_X32_2

        function Array_Check_L2_2 (array1, array2) result (res)
           logical*2 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_L2_2

        function Array_Check_L4_2 (array1, array2) result (res)
           logical*4 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_L4_2

        function Array_Check_L8_2 (array1, array2) result (res)
           logical*8 array1(:,:), array2(:,:)
           logical res
        end function Array_Check_L8_2

        function Array_Check_C_2 (array1, array2) result (res)
           character*(*) array1(:,:), array2(:,:)
           logical res
        end function Array_Check_C_2

     end interface Array_Check
