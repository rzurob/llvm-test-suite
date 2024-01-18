!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: implieddo001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be io-implied-do
!*                               Sequential Access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
   end type

   type,extends(base) :: child
      character(3) :: c1 = ''
   end type

end module


program implieddo001
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable     :: b1(:)
   class(base), allocatable     :: b2(:,:)
   type(base) :: b3(4)
   type(base),  pointer :: b4(:,:)
   integer :: stat
   character(200) :: msg
   character(12)  :: c1
   character(24)  :: c2
   character(6)   :: c3
   character(3)   :: c4

   ! allocation of variables
   allocate ( b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate ( b2(2,2), source = reshape (source = (/ child('ABC','abc'), child('DEF','def'), child('GHI','ghi'), child('JKL','jkl')  /), shape=(/2,2/) ) )
   b3 = (/ base('mno'), base('pqr'), base('stu'), base('vwx') /)
   allocate ( b4(2,2), source = reshape (source = (/ base('MNO'), base('PQR'), base('STU'), base('VWX')  /), shape=(/2,2/) ) )

   open (unit = 1, file ='implieddo001.1', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             ( b1, i= 1, 1, -1 )   !<- iteration count is 1
   write (1, iostat=stat, iomsg=msg )             ( ( b2(i,j), i = 1, 2), j = 1, 2 )
   write (1, iostat=stat, iomsg=msg )             ( b3(k), k=4_4,2_2,-2_8 )
   write (1, iostat=stat, iomsg=msg )             ( b4(2,2), j=1,2,3 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly
   if ( c1 /= 'abcdefghijkl' )                            error stop 1_4
   if ( c2 /= 'ABCabcDEFdefGHIghiJKLjkl' )                error stop 2_4
   if ( c3 /= 'vwxpqr' )                                  error stop 3_4
   if ( c4 /= 'VWX' )                                     error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    select type ( dtv )
       type is (child)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%c1
    end select

end subroutine
