!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saBasicStatus
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-27
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : basic test of progress + status
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*
!*  DESCRIPTION
!*
!*  Just like saBasic, except that we include the status code in the SYNC ALL statement.
!*  Also, to test the coexistence of SYNC ALL statements with and without sync-stat options,
!*  we include a second SYNC ALL statement.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saBasicStatus

   implicit none
   integer :: self, nImages, out, status
   character(4) :: outfile

   self = this_image()
   nImages = num_images()

   ! create log file for each image
   out = self+10
   write(outfile,'("o",i3.3)') out
   open(out,file=outfile,action='WRITE',status='REPLACE')

   write(out,*) 'Before'

   sync all (stat=status)

   write(out,*) 'After'

   if (status /= 0) write(out,*) 'Non-zero status:', status

   flush(out)
   close(out)

   if (self == 1) then
      print *, nImages
   end if

   sync all

end program saBasicStatus
