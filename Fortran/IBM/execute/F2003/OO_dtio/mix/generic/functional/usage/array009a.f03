!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type with multiple level of heirarchy, and use parent component in child dtio
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

program array009a
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1(:)
   class(child), pointer    :: c1(:,:)

   open ( 1, file = 'array009a.1', form='unformatted', access='sequential' )

   allocate ( b1(3), source = (/ child ( 'abc', 101 ), child ( 'def', 102 ), child ( 'ghi', 103 ) /) )
   allocate ( c1(2,2), source = reshape ( source = (/ gen3( 'abc', 201, 'ABC' ), gen3( 'def', 202, 'DEF' ), &
                                                      gen3( 'ghi', 203, 'GHI' ), gen3( 'jkl', 204, 'JKL' ) /), shape = (/2,2/)))

   write ( 1,  iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( b1(2), source = (/ gen3( 'abc', 301, 'ABC' ), gen3( 'def', 302, 'DEF' ) /) )

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( child :: b1(3) )
   allocate ( gen3 :: c1(2,2) )

   read ( 1, iostat = stat, iomsg = msg )    b1, c1

   select type ( b1 )
      type is ( child )
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 101 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 102 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 103 ) ) error stop 1_4
   end select

   select type ( c1 )
      type is ( gen3 )
        if ( ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%i /= 201 ) .or. ( c1(1,1)%s /= 'ABC' ) .or. &
             ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%i /= 202 ) .or. ( c1(2,1)%s /= 'DEF' ) .or. &
             ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%i /= 203 ) .or. ( c1(1,2)%s /= 'GHI' ) .or. &
             ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%i /= 204 ) .or. ( c1(2,2)%s /= 'JKL' )  ) error stop 2_4
   end select

   deallocate ( b1 )
   allocate ( gen3 :: b1(2) )

   read ( 1, iostat = stat, iomsg = msg ) b1

   select type ( b1 )
      type is ( gen3 )
        if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 301 ) .or. ( b1(1)%s /= 'ABC' ) .or. &
             ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 302 ) .or. ( b1(2)%s /= 'DEF' ) ) error stop 3_4
   end select

   close ( 1, status ='delete')

end program
