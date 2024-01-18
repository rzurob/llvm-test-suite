! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.* 
! %COMPOPTS: -qautodbl=dblpad4
! %GROUP: fxstio208a.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxstio208a.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 25, 2003
!*
!*  Primary Function Tested    : options with stream I/O 
!*
!*  Description                : Test POS, NUM, SIZE specifer in READ
!*                               WRITE & INQURE statments with      
!*                               -qautodbl=dblpad4 in stream I/O
!*
!***********************************************************************

      integer ios
      integer num(10) /10*-1/
      integer fsize(2) /2*-1/, pos(2) /2*-1/
      complex(8) x16 /(1.123456789d0,2.123456789d0)/, x16_var
      complex(4) x8, x8_var
      real(4) r4(2), r4_var(2)
      integer(8) i8(2), i8_var(2)
      equivalence(x16,x8,r4,i8)

      x16_var = x16
      x8_var = x8
      r4_var  = r4
      i8_var = i8

      open(1, form='unformatted', access='stream', &
         iostat=ios, err=100)

      write(1, iostat=ios, err=200) x16_var, x8_var, r4_var, i8_var

      open(2, form='unformatted', access='stream', iostat=ios, &
         err=100)

      call sub1(x16, x8, r4, i8, 1)

      write (2, iostat=ios, err=200) x16_var,  &
        x8_var, r4_var, i8_var


      call sub1(x16, x8, r4, i8, 2)

      if(num(1) .ne. num(5))   error stop 112
      if(num(2) .ne. num(6))   error stop 113
      if(num(3) .ne. num(7))   error stop 114
      if(num(4) .ne. num(8))   error stop 116
      if(pos(1) .ne. pos(2))   error stop 117
      if(fsize(1) .ne. fsize(2))   error stop 118
      
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
         complex(8) x16_var, x16, expect_x16 /(0.0d0,0.0d0)/
         complex(4) x8_var, x8, expect_x8 /(0.0e0,0.0e0)/
         real(4) r4_var(2), r4(2), expect_r4(2) /2*0.0/
         integer(8) i8_var(2), i8(2), expect_i8(2) /2*0/
         logical correct_x16, precision_r8, precision_x8
         integer num(10) /10*-1/
         integer fsize(2) /2*-1/, pos(2) /2*-1/

         if (flag .eq. 1) then
            rewind(1)
            read(1, num=num(1), iostat=ios, err=400) x16_var
            read(1, num=num(2), iostat=ios, err=400) x8_var
            read(1, num=num(3), iostat=ios, err=400) r4_var
            read(1, num=num(4), iostat=ios, err=400) i8_var
            inquire(1, pos=pos(1),size=fsize(1), iostat=ios, err=300)
            expect_x16 = x16
            expect_x8 = x8
            expect_r4 = r4
            expect_i8 = i8

         else if (flag .eq. 2) then
            read(2, num=num(5), iostat=ios, err=400) x16_var 
            read(2, num=num(6), iostat=ios, err=400) x8_var
            read(2, num=num(7), iostat=ios, err=400) r4_var
            read(2, num=num(8), iostat=ios, err=400) i8_var
            inquire(2, pos=pos(2),size=fsize(2), iostat=ios, err=300)
            expect_x16 = x16
            expect_x8 = x8
            expect_r4 = r4
            expect_i8 = i8
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


