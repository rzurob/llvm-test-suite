!.... This is a fake xlfutility module

      Module xlfutility
        integer(4), parameter :: TIME_SIZE = 8

      contains 
          integer(4) function alarm_(time, func)
            integer(4), intent(in) ::  time
            integer(4) func
            print *, "You've caled a fake xlfutility module function."
            alarm_=1313
          end function alarm_

          subroutine bic_(x1,x2)
           integer(4), intent(in) :: x1
            integer(4), intent(inout) :: x2
            print *, "You've caled a fake xlfutility module function."
          end subroutine bic_

          subroutine bis_(x1,x2)
            integer(4), intent(in) :: x1
            integer(4), intent(inout) :: x2
            print *, "You've caled a fake xlfutility module function."
          end subroutine bis_

          logical(4) function bit_(x1,x2)
            integer(4), intent(in) :: x1
            integer(4), intent(in) :: x2
            print *, "You've caled a fake xlfutility module function."
            bit_=.false.
          end function bit_

          character(8) function clock_()
            print *, "You've caled a fake xlfutility module function."
            clock_='99999999'
          end function clock_

          subroutine ctime_(str, time)
            integer(4), parameter :: TIME_SIZE = 8
            character(*), intent(out) :: str
            integer(kind=TIME_SIZE), intent(in) :: time
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine ctime_

          character(8) function date()
            print *, "You've caled a fake xlfutility module function."
            date='99999999'
          end function date

          real(4) function dtime_(dtime_struct)
            type tb_type
              sequence
              real(4) usrtime
              real(4) systime
            end type
            type(tb_type), intent(out) :: dtime_struct
            print *, "You've caled a fake xlfutility module function."
            dtime_=13.13
          end function dtime_

          real(4) function etime_(etime_struct)
            type tb_type
              sequence
              real(4) usrtime
              real(4) systime
            end type
            type(tb_type), intent(out) :: etime_struct
            print *, "You've caled a fake xlfutility module function."
            etime_=13.13
          end function etime_

          subroutine exit_(exit_status)
            integer(4), intent(in) :: exit_status
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine exit_

          subroutine fdate_(str)
            character(*), intent(out) :: str
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine fdate_

          subroutine flush_(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine flush_

          subroutine fpgets(fpstat)
            logical(4), intent(out) :: fpstat(32)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine fpgets

          subroutine fpsets(fpstat)
            logical(4), intent(in) :: fpstat(32)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine fpsets

          subroutine getarg(i1,c1)
            integer(4), intent(in) :: i1
            character(*), intent(out) :: c1
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine getarg

          integer(4) function getcwd_(name)
            character(*), intent(out) :: name
            print *, "You've caled a fake xlfutility module function."
            getcwd_=1313
          end function getcwd_

          integer(4) function getfd(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            getfd=1313
          end function getfd

          integer(4) function getgid_()
            print *, "You've caled a fake xlfutility module function."
            getgid_=1313 
          end function getgid_

          subroutine getlog_(name)
            character(*), intent(out) :: name
          end subroutine getlog_

          integer(4) function getpid_()
            print *, "You've caled a fake xlfutility module function."
            getpid_=1313
          end function getpid_

          integer(4) function getuid_()
            print *, "You've caled a fake xlfutility module function."
            getuid_=1313
          end function getuid_

          subroutine gmtime_(stime, tarray)
            integer(4), parameter :: TIME_SIZE = 8
            integer(kind=TIME_SIZE), intent(in) :: stime
            integer(4), intent(out) :: tarray(9)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine gmtime_

          integer(4) function hostnm_(name)
           character(*), intent(out) :: name
            print *, "You've caled a fake xlfutility module function."
            hostnm_=1313
          end function hostnm_

          integer(4) function iargc()
            print *, "You've caled a fake xlfutility module function."
            iargc=1313
          end function iargc

          subroutine idate_(idate_struct)
            type idate_type
              sequence
              integer(4) iday
              integer(4) imonth
              integer(4) iyear
            end type
            type(idate_type), intent(out) :: idate_struct
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine idate_

          integer(4) function ierrno_()
            print *, "You've caled a fake xlfutility module function."
            ierrno_=1313
          end function ierrno_

          integer(4) function irand()
            print *, "You've caled a fake xlfutility module function."
            irand=1313
          end function irand

          integer(8) function irtc()
            print *, "You've caled a fake xlfutility module function."
            irtc=1313
          end function irtc

          subroutine itime_(itime_struct)
            type iar
              sequence
              integer(4) ihr
              integer(4) imin
              integer(4) isec
            end type
            type(iar), intent(out) :: itime_struct
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine itime_

          character(8) function jdate()
            print *, "You've caled a fake xlfutility module function."
            jdate='999999999'
          end function jdate

          integer(4) function lenchr_(str)
            character(*), intent(in) :: str
            print *, "You've caled a fake xlfutility module function."
            lenchr_=1313
          end function lenchr_

          integer(4) function lnblnk_(str)
            character(*), intent(in) :: str
            print *, "You've caled a fake xlfutility module function."
            lnblnk_=1313
          end function lnblnk_

          subroutine ltime_(stime, tarray)
            integer(4), parameter :: TIME_SIZE = 8
            integer(kind=TIME_SIZE), intent(in) :: stime
            integer(4), intent(out) :: tarray(9)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine ltime_

          integer(4) function mclock()
            print *, "You've caled a fake xlfutility module function."
            mclock=1313
          end function mclock

          real(8) function rtc()
            print *, "You've caled a fake xlfutility module function."
            rtc=13.13
          end function rtc

          subroutine setrteopts(c1)
            character(*), intent(in) :: c1
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine setrteopts

          subroutine sleep_(sec)
            integer(4), intent(in) :: sec
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine sleep_

          real(8) function timef()
            print *, "You've caled a fake xlfutility module function."
            timef=13.13 
          end function timef

          function time_()
            integer(4), parameter :: TIME_SIZE = 8
            integer(kind=TIME_SIZE) time_
            print *, "You've caled a fake xlfutility module function."
            time_=1313
          end function time_

          integer(4) function umask_(cmask)
            integer(4), intent(in) :: cmask
            print *, "You've caled a fake xlfutility module function."
            umask_=1313
          end function umask_

          integer(4) function usleep_(msec)
            integer(4), intent(in) :: msec
            print *, "You've caled a fake xlfutility module function."
            usleep_=1313
          end function usleep_
     
          integer(4) function ftell_(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            ftell_=1313
          end function ftell_

          integer(8) function ftell64_(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            ftell64_=1313
          end function ftell64_


      end module
