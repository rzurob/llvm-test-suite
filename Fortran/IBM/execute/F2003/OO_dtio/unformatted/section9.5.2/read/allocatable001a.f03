! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an allocatable array
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
      character(2) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(2) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(2), intent(in) :: char
      a%c = char
   end subroutine
end module

program allocatable001a
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable :: b1(:), b2
   class(base), allocatable :: b3(:,:)
   type(base),  allocatable :: b4(:)
   integer :: stat
   character(200) :: msg
   character(4)   :: c1
   character(6)   :: c2
   character(14)  :: c3
   character(14)  :: c4

   ! allocation of variables
   allocate ( b1(2), source = (/ base('xx'), base('xx') /)  )  !<- 1 dimensional array
   allocate ( b2, source = base('xx') )                        !<- scalar
   allocate ( b3(2,2), source = reshape (source=(/base('xx'),base('xx'),base('xx'),base('xx') /), shape=(/2,2/)) )   !<- multi-dimensional array
   allocate ( b4(0), source = (/ (base('xx'),i=1,0)/) )        !<- zero sized array

   c1 = 'abcd'
   c2 = 'abcdef'
   c3 = 'abcdefghijklmn'
   c4 = 'abcdefghijklmn'

   open (unit = 1, file ='allocatable001a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              c1
   write (1, iostat=stat, iomsg=msg )              c2
   write (1, iostat=stat, iomsg=msg )              c3
   write (1, iostat=stat, iomsg=msg )              c4
   write (1, iostat=stat, iomsg=msg )              'a'

   rewind 1

   read (1, iostat=stat, iomsg=msg )               b1                   !<- shall read 'abcd'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 1_4
      msg = ''
      if ( ( b1(1)%c /= 'ab' ) .or. ( b1(2)%c /= 'cd' ) )               error stop 2_4

   read (1, iostat=stat, iomsg=msg )               b2, b1               !<- shall read 'ab' and 'cdef'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
      if ( ( b2%c    /= 'ab' )                                 &
      .or. ( b1(1)%c /= 'cd' ) .or. ( b1(2)%c /= 'ef' )        )        error stop 4_4

   read (1, iostat=stat, iomsg=msg )               b3, b2, b1           !<- shall read 'abcdefgh' and 'ij' and 'klmn'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 5_4
      msg = ''
      if ( ( b3(1,1)%c /= 'ab' ) .or. ( b3(2,1)%c /= 'cd' )    &
      .or. ( b3(1,2)%c /= 'ef' ) .or. ( b3(2,2)%c /= 'gh' )    &
      .or. ( b2%c      /= 'ij' )                               &
      .or. ( b1(1)%c   /= 'kl' ) .or. ( b1(2)%c /= 'mn' )      )        error stop 6_4

   read (1, iostat=stat, iomsg=msg )               b4, b1, b2, b3       !<- shall read '' and 'abcd' and 'ef' and 'ghijklmn'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 7_4
      msg = ''
      if ( ( b1(1)%c   /= 'ab' ) .or. ( b1(2)%c /= 'cd' )      &
      .or. ( b2%c      /= 'ef' )                               &
      .or. ( b3(1,1)%c /= 'gh' ) .or. ( b3(2,1)%c /= 'ij' )    &
      .or. ( b3(1,2)%c /= 'kl' ) .or. ( b3(2,2)%c /= 'mn' )    )        error stop 8_4

   read (1, iostat=stat, iomsg=msg )               b4                   !<- zero sized array shall not call dtio procedure
      if ( (stat /= 0) .or. (msg == 'dtio') )                           error stop 9_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(2) :: temp

   read (unit, iostat=iostat ) temp

   call dtv%setC(temp)

   iomsg = 'dtio'

end subroutine
