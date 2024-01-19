!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-07-26
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : basic test of progress
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*
!*  DESCRIPTION
!*
!*  Verify that all images complete if a SYNC ALL is encountered.
!*  We are explicitly interested only in achieving progress, i.e., that no
!*  image hangs.  We are not worried about whether the images actually wait
!*  at the SYNC ALL.
!*  We test this by printing "Before" and "After" before and after a SYNC ALL
!*  statement.  Each image prints one such line to a unit opened on a file
!*  unique to that image (and different from any standard unit).  Image 1 also
!*  prints the number of images to the standard output.  Outside this
!*  program we verify that all images have printed both statements.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saBasic

   implicit none
   integer :: self, nImages, out
   character(4) :: outfile

   self = this_image()
   nImages = num_images()

   ! create log file for each image
   out = self+10
   write(outfile,'("o",i3.3)') out
   open(out,file=outfile,action='WRITE',status='REPLACE')

   write(out,*) 'Before'

   sync all

   write(out,*) 'After'

   flush(out)
   close(out)

   if (self == 1) then
      print *, nImages
   end if

end program saBasic
