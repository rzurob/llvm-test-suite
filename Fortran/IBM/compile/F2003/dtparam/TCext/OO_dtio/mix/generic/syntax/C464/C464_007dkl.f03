!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C464 If generic-spec is dtio-generic-spec, the interface of each binding
!*                                                  shall be as specified in 9.5.3.7. The type of the dtv argument
!*                                                  shall be type-name.
!*
!*                                             - Characteristics of the dummy arguments
!*                                                -> allocatable, pointer, and target attributes
!*                               adaptation: exposed length
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

   type ::  base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1

      character(lbase_1) :: c

      contains

         procedure, pass :: uread
         procedure, pass :: uwrite
         procedure, pass :: fread
         procedure, pass :: fwrite

         generic, private :: read(unformatted)  => uread
         generic, private :: write(unformatted) => uwrite
         generic :: read(formatted)    => fread
         generic :: write(formatted)   => fwrite

   end type

   contains

      subroutine uread (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout), pointer :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine uwrite (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in), target :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout), allocatable :: dtv ! tcx: (*)
         integer, intent(in), target :: unit
         character(*), intent(in), target :: iotype
         integer, intent(in), target  :: v_list(:)
         integer, intent(out), target :: iostat
         character(*), intent(inout), target :: iomsg

         read (unit,*, iostat = iostat ) dtv%c

      end subroutine

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in), pointer :: unit
         character(*), intent(in), allocatable :: iotype
         integer, intent(in), allocatable  :: v_list(:)
         integer, intent(out), pointer :: iostat
         character(*), intent(inout), pointer :: iomsg

         write (unit,*, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

end module

program C464_007dkl
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
