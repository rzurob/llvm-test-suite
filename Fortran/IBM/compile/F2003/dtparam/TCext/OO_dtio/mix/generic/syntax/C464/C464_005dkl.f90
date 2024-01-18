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
!*                                                -> shape, and intent
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
         class(base(*)), intent(in) :: dtv          !<- should be intent(inout) ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

      end subroutine

      subroutine uwrite (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv       !<- should be intent(in) ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(inout) :: iostat        !<- should be intent(out)
         character(*), intent(in) :: iomsg       !<- should be intent(inout)

         write (unit, iostat = iostat ) dtv%c

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv(:)    !<- should be scalar ! tcx: (*)
         integer, intent(inout) :: unit          !<- should be intent(in)
         character(*), intent(in) :: iotype(:)   !<- should be scalar
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat(:)       !<- should be scalar
         character(*), intent(inout) :: iomsg(:) !<- should be scalar

         read (unit,* ) dtv%c

      end subroutine

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:,:,:)      !<- should be 1-d array
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit,*, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

end module

program C464_005dkl
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
