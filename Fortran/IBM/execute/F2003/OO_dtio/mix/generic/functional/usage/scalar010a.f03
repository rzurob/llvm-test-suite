!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar derived type with multiple level of heirarchy, and use parent component in child dtio
!*                                    with unformatted I/O
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
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(3) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%base, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%base, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%child, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iostat, iomsg)
         class(gen3), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%child, dtv%s
         iomsg = 'dtioreadg'

      end subroutine

end module

program scalar010a
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1
   class(child), pointer    :: c1

   open ( 1, file = 'scalar010a.1', form='unformatted', access='sequential' )

   allocate ( b1, source = child ( 'abc', 101 ) )
   allocate ( c1, source = gen3  ( 'def', 102, 'DEF' ) )

   write ( 1, iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( b1, source = gen3 ( 'ghi', 103, 'GHI' ) )

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( child :: b1 )
   allocate ( gen3 :: c1 )

   read ( 1, iostat = stat, iomsg = msg ) b1, c1

   select type ( b1 )
      type is ( child )
        if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 101 ) ) error stop 1_4
   end select

   select type ( c1 )
      type is ( gen3 )
        if ( ( c1%c /= 'def' ) .or. ( c1%i /= 102 ) .or. ( c1%s /= 'DEF' ) ) error stop 2_4
   end select

   deallocate ( b1 )
   allocate ( b1, source = gen3 () )

   read ( 1, iostat = stat, iomsg = msg ) b1

   select type ( b1 )
      type is ( gen3 )
        if ( ( b1%c /= 'ghi' ) .or. ( b1%i /= 103 ) .or. ( b1%s /= 'GHI' ) ) error stop 3_4
   end select

   close ( 1, status ='delete')

end program