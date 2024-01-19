      ! ASYNCHRONOUS statement is legal within block data block
       block data aa
         integer :: ii
         asynchronous :: ii
       end block data
       end
