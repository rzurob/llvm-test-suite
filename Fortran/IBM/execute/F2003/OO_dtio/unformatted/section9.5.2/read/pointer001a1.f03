! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2:
!*                               zero-sized pointer shall not invoked DTIO procedure (read)
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
   end type

   type, extends(base) :: child
      character(2) :: c1 = ''
   end type

end module


program pointer001a1
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
   class(base), pointer :: b1(:), b2(:,:)

   class(base), allocatable, target :: b3(:), b5(:)
   type(child), allocatable, target :: b4(:,:), b6(:,:)

   integer :: stat
   character(200) :: msg = ''
   character(1)   :: c1, c2, c3, c4

   ! allocation of variables
   allocate ( b3(4),   source = (/ child('ab','AB'), child('cd','CD'), child('ef','EF'), child('gh','GH') /))
   allocate ( b4(2,2), source = reshape( source = (/ child('ij','IJ'), child('kl','KL'), child('mn','MN'), child('op','OP') /), shape=(/2,2/) ) )
   allocate ( b5(4),   source = (/ child('AB','ab'), child('CD','cd'), child('EF','ef'), child('GH','gh') /))
   allocate ( b6(2,2), source = reshape( source = (/ child('IJ','ij'), child('KL','kl'), child('MN','mn'), child('OP','op') /), shape=(/2,2/) ) )


   open (unit = 1, file ='pointer001a1.data', form='unformatted', access='sequential')

   b1 => b3(4:1:2)   !<- zero-sized
   b2 => b4(2:1,2:1) !<- zero-sized

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              'a'
   if (stat /= 0 ) error stop 5_4
   write (1, iostat=stat, iomsg=msg )              'b'
   if (stat /= 0 ) error stop 6_4
   write (1, iostat=stat, iomsg=msg )              'c'
   if (stat /= 0 ) error stop 7_4
   write (1, iostat=stat, iomsg=msg )              'd'
   if (stat /= 0 ) error stop 8_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )             c1, b1    !<- shall not call DTIO
   if ( (stat /= 0) .or. (msg /= '' )) error stop 1_4
   read (1, iostat=stat, iomsg=msg )             c2, b2    !<- shall not call DTIO
   if ( (stat /= 0) .or. (msg /= '' )) error stop 2_4

   b1 => b5(1:2:-1)     !<- zero-sized
   b2 => b6(1:2:-1,1:2) !<- zero-sized

   read (1, iostat=stat, iomsg=msg )             c3, b1    !<- shall not call DTIO
   if ( (stat /= 0) .or. (msg /= '' )) error stop 3_4
   read (1, iostat=stat, iomsg=msg )             c4, b2    !<- shall not call DTIO
   if ( (stat /= 0) .or. (msg /= '' )) error stop 4_4

   ! check if the values are set correctly

   if ( c1 /= "a" )                  error stop 9_4
   if ( c2 /= "b" )                  error stop 10_4
   if ( c3 /= "c" )                  error stop 11_4
   if ( c4 /= "d" )                  error stop 12_4


   select type ( b3 )
      type is (child)
      if ( ( b3(1)%c /= 'ab' ) .or.  ( b3(1)%c1 /= 'AB' ) .or. ( b3(2)%c /= 'cd' ) .or.  ( b3(2)%c1 /= 'CD' ) .or. &
           ( b3(3)%c /= 'ef' ) .or.  ( b3(3)%c1 /= 'EF' ) .or. ( b3(4)%c /= 'gh' ) .or.  ( b3(4)%c1 /= 'GH' ) )       error stop 13_4
   end select

   if ( ( b4(1,1)%c /= 'ij' ) .or.  ( b4(1,1)%c1 /= 'IJ' ) .or. ( b4(2,1)%c /= 'kl' ) .or.  ( b4(2,1)%c1 /= 'KL' ) .or. &
        ( b4(1,2)%c /= 'mn' ) .or.  ( b4(1,2)%c1 /= 'MN' ) .or. ( b4(2,2)%c /= 'op' ) .or.  ( b4(2,2)%c1 /= 'OP' ) )  error stop 14_4

   select type ( b5 )
      type is (child)
      if ( ( b5(1)%c1 /= 'ab' ) .or.  ( b5(1)%c /= 'AB' ) .or. ( b5(2)%c1 /= 'cd' ) .or.  ( b5(2)%c /= 'CD' ) .or. &
           ( b5(3)%c1 /= 'ef' ) .or.  ( b5(3)%c /= 'EF' ) .or. ( b5(4)%c1 /= 'gh' ) .or.  ( b5(4)%c /= 'GH' ) )       error stop 15_4
   end select

   if ( ( b6(1,1)%c1 /= 'ij' ) .or.  ( b6(1,1)%c /= 'IJ' ) .or. ( b6(2,1)%c1 /= 'kl' ) .or.  ( b6(2,1)%c /= 'KL' ) .or. &
        ( b6(1,2)%c1 /= 'mn' ) .or.  ( b6(1,2)%c /= 'MN' ) .or. ( b6(2,2)%c1 /= 'op' ) .or.  ( b6(2,2)%c /= 'OP' ) )  error stop 16_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    error stop 17_4

end subroutine