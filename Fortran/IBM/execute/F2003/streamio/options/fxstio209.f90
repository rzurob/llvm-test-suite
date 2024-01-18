! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS: -qautodbl=dblpad8
! %GROUP: fxstio209.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 25, 2003
!*
!*  Primary Function Tested    : options with stream I/O
!*
!*  Description                : Test POS, NUM, SIZE specifer in READ
!*                               WRITE & INQURE statments with
!*                               -qautodbl=dblpad8 in stream I/O
!*
!***********************************************************************

      integer ios
      integer num(10) /10*-1/
      integer fsize(2) /2*-1/, pos(2) /2*-1/
      complex(8) x16 /(1.123456789d0,2.123456789d0)/, x16_var
      complex(16) x8, x8_var
      real(8) r4(2), r4_var(2)
      integer(8) i8(4), i8_var(4)
      equivalence(x16,x8,r4,i8)

      x16_var = x16
      x8_var = x8
      r4_var  = r4
      i8_var = i8

      open(1, form='unformatted', access='stream', &
         iostat=ios, err=100)

      write(1, iostat=ios, err=200) x16_var, x8_var, r4_var, i8_var

      open(2, form='formatted', access='stream', iostat=ios, &
         err=100)

      call sub1(x16, x8, r4, i8, 1)

      write (2, FMT='(2E15.7,2E15.7,D25.17,I20)', iostat=ios, err=200) &
         x16_var,  x8_var, r4_var, i8_var

      call sub1(x16, x8, r4, i8, 2)

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
300 print *, "inquire error: iostat = ", ios
    error stop 300
400 print *, "read error: iostat = ", ios
    error stop 400
500 print *, "rewind error: iostat = ", ios
    error stop 500

      end

      subroutine sub1(x16, x8, r4, i8, flag)
         complex(8) x16_var,x16, expect_x16 /(0.0d0,0.0d0)/
         complex(4) x8_var, x8, expect_x8 /(0.0e0,0.0e0)/
         real(4) r4_var(2), r4(2), expect_r4(2) /2*0.0/
         integer(8) i8_var(2), i8(2), expect_i8(2) /2*0/
         logical correct_x8, precision_r6, precision_x6
         integer num(4) /4*-1/
         integer fsize /-1/, fpos /-1/

         if (flag .eq. 1) then
            rewind(1)
            read(1, num=num(1), iostat=ios, err=400) x16_var
            read(1, num=num(2), iostat=ios, err=400) x8_var
            read(1, num=num(3), iostat=ios, err=400) r4_var
            read(1, num=num(4), iostat=ios, err=400) i8_var
            inquire(1, pos=fpos,size=fsize, iostat=ios, err=300)
            expect_x16 = x16
            expect_x8 = x8
            expect_r4 = r4
            expect_i8 = i8
         if(num(1) .ne. num(2))      error stop 222
         if(num(3) .ne. num(4))      error stop 223
         if(fsize  .ne. num(1)*4)    error stop 224
         if(fpos   .ne. (num(1)*4+1)) error stop 225
            close(1)
         else if (flag .eq. 2) then
            read(2, fmt='(2E15.7)', iostat=ios, err=400) x16_var
            read(2, fmt='(2E15.7)', iostat=ios, err=400) x8_var
            read(2, fmt='(D25.17)', iostat=ios, err=400) r4_var
            read(2, fmt='(I20)',    iostat=ios, err=400) i8_var
            inquire(2, pos=fpos,size=fsize, iostat=ios, err=300)
            expect_x16 = x16
            expect_x8 = x8
            expect_r4 = r4
            expect_i8 = i8
            if(fsize .ne. 4*16)      error stop 226
            if(fpos  .ne. 4*16+1)    error stop 227
            close(2)
         endif

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
300 print *, "inquire error: iostat = ", ios
    error stop 300
400 print *, "read error: iostat = ", ios
    error stop 400
500 print *, "rewind error: iostat = ", ios
    error stop 500

      end

