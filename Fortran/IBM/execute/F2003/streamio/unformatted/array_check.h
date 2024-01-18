       logical function cmp_i1_array(rslt,expect,size)
          integer*4 size
          integer*1 rslt(*),expect(*)
          cmp_i1_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_i1_array=.false.
                return
             end if
          end do
       end function cmp_i1_array

       logical function cmp_i2_array(rslt,expect,size)
          integer*4 size
          integer*2 rslt(*),expect(*)
          cmp_i2_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_i2_array=.false.
                return
             end if
          end do
       end function cmp_i2_array

       logical function cmp_i4_array(rslt,expect,size)
          integer*4 size
          integer*4 rslt(*),expect(*)
          cmp_i4_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_i4_array=.false.
                return
             end if
          end do
       end function cmp_i4_array

       logical function cmp_i8_array(rslt,expect,size)
          integer*4 size
          integer*8 rslt(*),expect(*)
          cmp_i8_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_i8_array=.false.
                return
             end if
          end do
       end function cmp_i8_array


       logical function cmp_r4_array(rslt,expect,size)
          integer*4 size
          real*4 rslt(*),expect(*)
          logical precision_r4
          cmp_r4_array=.true.
          do i=1,size
             if (.not.precision_r4(rslt(i),expect(i))) then
                cmp_r4_array=.false.
                return
             end if
          end do
       end function cmp_r4_array


       logical function cmp_r8_array(rslt,expect,size)
          integer*4 size
          real*8 rslt(*),expect(*)
          logical precision_r8
          cmp_r8_array=.true.
          do i=1,size
             if (.not.precision_r8(rslt(i),expect(i))) then
                cmp_r8_array=.false.
                return
             end if
          end do
       end function cmp_r8_array


       logical function cmp_r16_array(rslt,expect,size)
          integer*4 size
          real*16 rslt(*),expect(*)
          logical precision_r6
          cmp_r16_array=.true.
          do i=1,size
             if (.not.precision_r6(rslt(i),expect(i))) then
                cmp_r16_array=.false.
                return
             end if
          end do
       end function cmp_r16_array


       logical function cmp_x8_array(rslt,expect,size)
          integer*4 size
          complex*8 rslt(*),expect(*)
          logical precision_x8
          cmp_x8_array=.true.
          do i=1,size
             if (.not.precision_x8(rslt(i),expect(i))) then
                cmp_x8_array=.false.
                return
             end if
          end do
       end function cmp_x8_array


       logical function cmp_x16_array(rslt,expect,size)
          integer*4 size
          complex*16 rslt(*),expect(*)
          logical precision_x6
          cmp_x16_array=.true.
          do i=1,size
             if (.not.precision_x6(rslt(i),expect(i))) then
                cmp_x16_array=.false.
                return
             end if
          end do
       end function cmp_x16_array


       logical function cmp_x32_array(rslt,expect,size)
          integer*4 size
          complex*32 rslt(*),expect(*)
          logical precision_x3
          cmp_x32_array=.true.
          do i=1,size
             if (.not.precision_x3(rslt(i),expect(i))) then
                cmp_x32_array=.false.
                return
             end if
          end do
       end function cmp_x32_array


       logical function cmp_l1_array(rslt,expect,size)
          integer*4 size
          logical*1 rslt(*),expect(*)
          cmp_l1_array=.true.
          do i=1,size
             if (rslt(i).neqv.expect(i)) then
                cmp_l1_array=.false.
                return
             end if
          end do
       end function cmp_l1_array

       logical function cmp_l2_array(rslt,expect,size)
          integer*4 size
          logical*2 rslt(*),expect(*)
          cmp_l2_array=.true.
          do i=1,size
             if (rslt(i).neqv.expect(i)) then
                cmp_l2_array=.false.
                return
             end if
          end do
       end function cmp_l2_array

       logical function cmp_l4_array(rslt,expect,size)
          integer*4 size
          logical*4 rslt(*),expect(*)
          cmp_l4_array=.true.
          do i=1,size
             if (rslt(i).neqv.expect(i)) then
                cmp_l4_array=.false.
                return
             end if
          end do
       end function cmp_l4_array


       logical function cmp_l8_array(rslt,expect,size)
          integer*4 size
          logical*8 rslt(*),expect(*)
          cmp_l8_array=.true.
          do i=1,size
             if (rslt(i).neqv.expect(i)) then
                cmp_l8_array=.false.
                return
             end if
          end do
       end function cmp_l8_array


       logical function cmp_b_array(rslt,expect,size)
          integer*4 size
          byte rslt(*),expect(*)
          cmp_b_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_b_array=.false.
                return
             end if
          end do
       end function cmp_b_array


       logical function cmp_ch_array(rslt,expect,size)
          integer*4 size
          character*(*) rslt(*),expect(*)
          cmp_ch_array=.true.
          do i=1,size
             if (rslt(i).ne.expect(i)) then
                cmp_ch_array=.false.
                return
             end if
          end do
       end function cmp_ch_array
