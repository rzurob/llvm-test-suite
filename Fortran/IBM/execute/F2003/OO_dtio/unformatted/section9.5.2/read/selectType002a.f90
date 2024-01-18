!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType002a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read selector (array/array section) in class default in select type construct
!*                               Sequential Access
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      character(3) :: cc = 'xxx'
   end type

end module


module m1
   use m

   type, extends(child) :: gen3
      integer(4) :: i = -999
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program selectType002a
   use m1

   ! declaration of variables
   class(base), pointer     :: b1(:)
   class(base), allocatable :: b2(:), b3(:,:)

   class(child), pointer    :: c1(:)
   class(child), allocatable:: c2(:,:)

   class(gen3), pointer     :: g1(:)

   integer :: stat
   character(200) :: msg =''

   ! allocation of variables

   allocate ( b1(2)   , source = (/ base(), base() /) )
   allocate ( b2(3)   , source = (/ child(), child(), child() /) )
   allocate ( b3(2,2) , source = reshape( source = (/ gen3(), gen3(), gen3(), gen3() /), shape = (/2,2/) ) )

   allocate ( c1(2), source = (/ child(), child() /) )
   allocate ( c2(2,2), source = reshape( source = (/ gen3(), gen3(), gen3(), gen3() /), shape = (/2,2/) ) )

   allocate ( g1(2), source = (/ gen3(), gen3() /))

   open (unit = 1, file ='selectType002a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations


   write (1, iostat=stat, iomsg=msg)       'abcdef'
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 1_4
   write (1, iostat=stat, iomsg=msg)       'abcdef','ghijkl'
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 2_4
   write (1, iostat=stat, iomsg=msg)       'abcdef', 101, 'ghijkl', 102, 'mnopqr', 103,'stuvwx',104
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 3_4
   write (1, iostat=stat, iomsg=msg)       'ABCDEF','GHIJKL'
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 4_4
   write (1, iostat=stat, iomsg=msg)       'ABCDEF', 201, 'GHIJKL', 202, 'MNOPQR', 203,'STUVWX',204
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 5_4
   write (1, iostat=stat, iomsg=msg)       'ABcDEf', 301, 'GHiJKl', 302
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 6_4

   rewind 1
   msg = ''

   ! check if the values are set correctly

   select type (b1)
      class default
         read (1, iostat=stat, iomsg=msg )    b1
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )         error stop 7_4
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) ) error stop 8_4
   end select

   select type (g => b2(1:3:2) )
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )  error stop 9_4
         select type ( b2 )
            type is ( child )
               if ( ( b2(1)%c /= 'abc' ) .or. ( b2(1)%cc /= 'def' ) .or. &
                    ( b2(2)%c /= 'xxx' ) .or. ( b2(2)%cc /= 'xxx' ) .or. &
                    ( b2(3)%c /= 'ghi' ) .or. ( b2(3)%cc /= 'jkl' ) ) error stop 10_4
            class default
               error stop 11_4
         end select
   end select

   select type ( g => b3 )
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )  error stop 12_4
         select type ( g )
            type is ( gen3 )
               if ( ( g(1,1)%c /= 'abc' ) .or. ( g(1,1)%cc /= 'def' ) .or. ( g(1,1)%i /= 101 ).or. &
                    ( g(2,1)%c /= 'ghi' ) .or. ( g(2,1)%cc /= 'jkl' ) .or. ( g(2,1)%i /= 102 ).or. &
                    ( g(1,2)%c /= 'mno' ) .or. ( g(1,2)%cc /= 'pqr' ) .or. ( g(1,2)%i /= 103 ).or. &
                    ( g(2,2)%c /= 'stu' ) .or. ( g(2,2)%cc /= 'vwx' ) .or. ( g(2,2)%i /= 104 ) ) error stop 13_4
            class default
               error stop 14_4
         end select
   end select

   select type (g => c1(1:2))
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( g(1)%c /= "ABC" ) .or. ( g(1)%cc /= "DEF" ) .or. &
              ( g(2)%c /= "GHI" ) .or. ( g(2)%cc /= "JKL" ) ) error stop 15_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )        error stop 16_4
   end select

   select type (c2)
      class default
         read (1, iostat=stat, iomsg=msg )    c2
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )  error stop 17_4
         select type ( c2 )
            type is ( gen3 )
               if ( ( c2(1,1)%c /= 'ABC' ) .or. ( c2(1,1)%cc /= 'DEF' ) .or. ( c2(1,1)%i /= 201 ).or. &
                    ( c2(2,1)%c /= 'GHI' ) .or. ( c2(2,1)%cc /= 'JKL' ) .or. ( c2(2,1)%i /= 202 ).or. &
                    ( c2(1,2)%c /= 'MNO' ) .or. ( c2(1,2)%cc /= 'PQR' ) .or. ( c2(1,2)%i /= 203 ).or. &
                    ( c2(2,2)%c /= 'STU' ) .or. ( c2(2,2)%cc /= 'VWX' ) .or. ( c2(2,2)%i /= 204 ) ) error stop 18_4
            class default
               error stop 19_4
         end select
   end select

   select type (g => g1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( g(1)%c /= "ABc" ) .or. ( g(1)%cc /= "DEf" ) .or. ( g(1)%i /= 301 ) .or. &
              ( g(2)%c /= "GHi" ) .or. ( g(2)%cc /= "JKl" ) .or. ( g(2)%i /= 302 ) ) error stop 20_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') )  error stop 21_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
       type is (base)
          read (unit, iostat=iostat ) dtv%c
       type is (child)
          read (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3)
          read (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtioread'

end subroutine
