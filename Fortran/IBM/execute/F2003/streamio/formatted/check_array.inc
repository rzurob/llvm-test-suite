
!**********************************************************
!*       One Dimensional Arrays                           *
!**********************************************************

   function Array_Check_I2 (array1, array2) result (res)
        integer*2 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .ne. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_I2
 
   function Array_Check_I4 (array1, array2) result (res)
        integer*4 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .ne. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_I4
 
   function Array_Check_I8 (array1, array2) result (res)
        integer*8 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .ne. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_I8
 

   function Array_Check_R4 (array1, array2) result (res)
        real*4 array1(:), array2(:)
        integer*4 i
        logical res, precision_R4

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_R4(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_R4
 
   function Array_Check_R8 (array1, array2) result (res)
        real*8 array1(:), array2(:)
        integer*4 i
        logical res, precision_R8

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_R8(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_R8
 
   function Array_Check_R16 (array1, array2) result (res)
        real*16 array1(:), array2(:)
        integer*4 i
        logical res, precision_R6

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_R6(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_R16
 

   function Array_Check_X8 (array1, array2) result (res)
        complex*8 array1(:), array2(:)
        integer*4 i
        logical res, precision_X8

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_X8(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_X8
 
   function Array_Check_X16 (array1, array2) result (res)
        complex*16 array1(:), array2(:)
        integer*4 i
        logical res, precision_X6

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_X6(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_X16
 
   function Array_Check_X32 (array1, array2) result (res)
        complex*32 array1(:), array2(:)
        integer*4 i
        logical res, precision_X3

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( .not. precision_X3(array1(i), array2(i)) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_X32
 
   function Array_Check_L2 (array1, array2) result (res)
        logical*2 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .neqv. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_L2
 
   function Array_Check_L4 (array1, array2) result (res)
        logical*4 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .neqv. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_L4
 
   function Array_Check_L8 (array1, array2) result (res)
        logical*8 array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .neqv. array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_L8
 
   function Array_Check_C (array1, array2) result (res)
        character*(*) array1(:), array2(:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            if ( array1(i) .ne.  array2(i) ) res = .false.
            i = i + 1
        end do
        return
    end function Array_Check_C
 

!**********************************************************
!*       Two Dimensional Arrays                           *
!**********************************************************

    function Array_Check_I2_2 (array1, array2) result (res)
        integer*2 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .ne. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_I2_2

    function Array_Check_I4_2 (array1, array2) result (res)
        integer*4 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .ne. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_I4_2

    function Array_Check_I8_2 (array1, array2) result (res)
        integer*8 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .ne. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_I8_2


    function Array_Check_R4_2 (array1, array2) result (res)
        real*4 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_R4

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_R4 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_R4_2
 

    function Array_Check_R8_2 (array1, array2) result (res)
        real*8 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_R8

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_R8 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_R8_2
 

    function Array_Check_R16_2 (array1, array2) result (res)
        real*16 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_R6

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_R6 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_R16_2
 

    function Array_Check_X8_2 (array1, array2) result (res)
        complex*8 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_X8

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_X8 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_X8_2
 

    function Array_Check_X16_2 (array1, array2) result (res)
        complex*16 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_X6

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_X6 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_X16_2
 

    function Array_Check_X32_2 (array1, array2) result (res)
        complex*32 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res, precision_X3

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( .not. precision_X3 (array1(i,j), array2(i,j)) ) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_X32_2
 

    function Array_Check_L2_2 (array1, array2) result (res)
        logical*2 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .neqv. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_L2_2


    function Array_Check_L4_2 (array1, array2) result (res)
        logical*4 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .neqv. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_L4_2


    function Array_Check_L8_2 (array1, array2) result (res)
        logical*8 array1(:,:), array2(:,:)
        integer*4 i, j
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .neqv. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_L8_2


    function Array_Check_C_2 (array1, array2) result (res)
        character*(*) array1(:,:), array2(:,:)
        integer*4 i
        logical res

        res = .true.
        if ( size(array1) .ne. size ( array2) ) res = .false.

        i = lbound(array1, 1)
        do while ( i .le. ubound(array1, 1) .and. res .eqv. .true. )
            j = lbound(array1, 2)
            do while ( j .le. ubound(array1, 2) .and. res .eqv. .true. )
               if ( array1(i,j) .ne. array2(i,j)) res = .false.
               j = j + 1
            end do
            i = i + 1
        end do
        return
    end function Array_Check_C_2
 
