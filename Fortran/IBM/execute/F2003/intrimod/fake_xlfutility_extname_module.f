!.... This is a fake xlfutility module

@process extname
      Module xlfutility_extname
        integer(4), parameter :: TIMESIZE = 8

      contains 
          integer(4) function alarm(time, func)
            integer(4), intent(in) ::  time
            integer(4) func
            print *, "You've caled a fake xlfutility module function."
            alarm=1313
          end function alarm

          subroutine bic(x1,x2)
           integer(4), intent(in) :: x1
            integer(4), intent(inout) :: x2
            print *, "You've caled a fake xlfutility module function."
          end subroutine bic

          subroutine bis(x1,x2)
            integer(4), intent(in) :: x1
            integer(4), intent(inout) :: x2
            print *, "You've caled a fake xlfutility module function."
          end subroutine bis

          logical(4) function bit(x1,x2)
            integer(4), intent(in) :: x1
            integer(4), intent(in) :: x2
            print *, "You've caled a fake xlfutility module function."
            bit=.false.
          end function bit

          character(8) function clock()
            print *, "You've caled a fake xlfutility module function."
            clock='99999999'
          end function clock

          subroutine ctime(str, time)
            integer(4), parameter :: TIMESIZE = 8
            character(*), intent(out) :: str
            integer(kind=TIMESIZE), intent(in) :: time
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine ctime

          character(8) function date()
            print *, "You've caled a fake xlfutility module function."
            date='99999999'
          end function date

          real(4) function dtime(dtime_struct)
            type tbtype
              sequence
              real(4) usrtime
              real(4) systime
            end type
            type(tbtype), intent(out) :: dtime_struct
            print *, "You've caled a fake xlfutility module function."
            dtime=13.13
          end function dtime

          real(4) function etime(etime_struct)
            type tbtype
              sequence
              real(4) usrtime
              real(4) systime
            end type
            type(tbtype), intent(out) :: etime_struct
            print *, "You've caled a fake xlfutility module function."
            etime=13.13
          end function etime

          subroutine exit(exit_status)
            integer(4), intent(in) :: exit_status
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine exit

          subroutine fdate(str)
            character(*), intent(out) :: str
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine fdate

          subroutine flush(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine flush

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

          integer(4) function getcwd(name)
            character(*), intent(out) :: name
            print *, "You've caled a fake xlfutility module function."
            getcwd=1313
          end function getcwd

          integer(4) function getfd(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            getfd=1313
          end function getfd

          integer(4) function getgid()
            print *, "You've caled a fake xlfutility module function."
            getgid=1313 
          end function getgid

          subroutine getlog(name)
            character(*), intent(out) :: name
          end subroutine getlog

          integer(4) function getpid()
            print *, "You've caled a fake xlfutility module function."
            getpid=1313
          end function getpid

          integer(4) function getuid()
            print *, "You've caled a fake xlfutility module function."
            getuid=1313
          end function getuid

          subroutine gmtime(stime, tarray)
            integer(4), parameter :: TIMESIZE = 8
            integer(kind=TIMESIZE), intent(in) :: stime
            integer(4), intent(out) :: tarray(9)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine gmtime

          integer(4) function hostnm(name)
           character(*), intent(out) :: name
            print *, "You've caled a fake xlfutility module function."
            hostnm=1313
          end function hostnm

          integer(4) function iargc()
            print *, "You've caled a fake xlfutility module function."
            iargc=1313
          end function iargc

          subroutine idate(idate_struct)
            type idatetype
              sequence
              integer(4) iday
              integer(4) imonth
              integer(4) iyear
            end type
            type(idatetype), intent(out) :: idate_struct
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine idate

          integer(4) function ierrno()
            print *, "You've caled a fake xlfutility module function."
            ierrno=1313
          end function ierrno

          integer(4) function irand()
            print *, "You've caled a fake xlfutility module function."
            irand=1313
          end function irand

          integer(8) function irtc()
            print *, "You've caled a fake xlfutility module function."
            irtc=1313
          end function irtc

          subroutine itime(itime_struct)
            type iar
              sequence
              integer(4) ihr
              integer(4) imin
              integer(4) isec
            end type
            type(iar), intent(out) :: itime_struct
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine itime

          character(8) function jdate()
            print *, "You've caled a fake xlfutility module function."
            jdate='999999999'
          end function jdate

          integer(4) function lenchr(str)
            character(*), intent(in) :: str
            print *, "You've caled a fake xlfutility module function."
            lenchr=1313
          end function lenchr

          integer(4) function lnblnk(str)
            character(*), intent(in) :: str
            print *, "You've caled a fake xlfutility module function."
            lnblnk=1313
          end function lnblnk

          subroutine ltime(stime, tarray)
            integer(4), parameter :: TIMESIZE = 8
            integer(kind=TIMESIZE), intent(in) :: stime
            integer(4), intent(out) :: tarray(9)
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine ltime

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

          subroutine sleep(sec)
            integer(4), intent(in) :: sec
            print *, "You've caled a fake xlfutility module subroutine."
          end subroutine sleep

          real(8) function timef()
            print *, "You've caled a fake xlfutility module function."
            timef=13.13 
          end function timef

          function time()
            integer(4), parameter :: TIMESIZE = 8
            integer(kind=TIMESIZE) time_
            print *, "You've caled a fake xlfutility module function."
            time=1313
          end function time

          integer(4) function umask(cmask)
            integer(4), intent(in) :: cmask
            print *, "You've caled a fake xlfutility module function."
            umask=1313
          end function umask

          integer(4) function usleep(msec)
            integer(4), intent(in) :: msec
            print *, "You've caled a fake xlfutility module function."
            usleep=1313
          end function usleep
     
          integer(4) function ftell(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            ftell=1313
          end function ftell

          integer(8) function ftell64(lunit)
            integer(4), intent(in) :: lunit
            print *, "You've caled a fake xlfutility module function."
            ftell64=1313
          end function ftell64


      end module
