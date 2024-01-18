!C807 A SAVE statement in a BLOCK construct shall contain a saved-entity-list
!that does not specify a common-block-name

  integer i
  common /comblk/ i
  block 
    save /comblk/
  end block
end 
