program aoc1_part2
  implicit none

  integer :: stat, answer=0
  character(80) :: line

  ! open input file
  open (10, file='input.txt', status='old', iostat=stat)
  !if (stat .ne. 0)then
  !  write (*,*) 'inp_file can not be opened'
  !  stop
  !end if
  
  main_loop: do while (.true.)
    read (10, '(A)', end=99) line
    block
      integer :: i, left, right
      left_loop: do i = 1,len_trim(line)
        if (.not. (line(i:i) < '0' .or. line(i:i) > '9')) then
            read(line(i:i),'(i1)') left
            exit left_loop
        else
          left = match_written_number_left(line(i:i+4))
          if (left>0) then
            ! print *, "left", left
            exit left_loop
          endif
        end if
      end do left_loop
      right_loop: do i = len_trim(line), 1, -1
        if (.not. (line(i:i) < '0' .or. line(i:i) > '9')) then
            read(line(i:i),'(i1)') right
            exit right_loop
        else
          right = match_written_number_right(line(i-4:i))
          if (right>0) then
            ! print *, "right", right
            exit right_loop
          endif
        end if
      end do right_loop
      !print *, "left", left, "right", right
    answer=answer+10*left+right
    end block
  enddo main_loop

  99 continue
  print *, answer

  contains

    pure integer function match_written_number_left (charray) result (n)
      character(5), intent(in) :: charray

      if (charray(1:3) .eq. 'one') then
        n = 1
      else if (charray(1:3) .eq. 'two') then
        n = 2
      else if (charray(1:5) .eq. 'three') then
        n = 3
      else if (charray(1:4) .eq. 'four') then
        n = 4
      else if (charray(1:4) .eq. 'five') then
        n = 5
      else if (charray(1:3) .eq. 'six') then
        n = 6
      else if (charray(1:5) .eq. 'seven') then
        n = 7
      else if (charray(1:5) .eq. 'eight') then
        n = 8
      else if (charray(1:4) .eq. 'nine') then
        n = 9
      else
        n = 0
      end if

    end function
    
    pure integer function match_written_number_right (charray) result (n)
      character(5), intent(in) :: charray

      if (charray(3:5) .eq. 'one') then
        n = 1
      else if (charray(3:5) .eq. 'two') then
        n = 2
      else if (charray(1:5) .eq. 'three') then
        n = 3
      else if (charray(2:5) .eq. 'four') then
        n = 4
      else if (charray(2:5) .eq. 'five') then
        n = 5
      else if (charray(3:5) .eq. 'six') then
        n = 6
      else if (charray(1:5) .eq. 'seven') then
        n = 7
      else if (charray(1:5) .eq. 'eight') then
        n = 8
      else if (charray(2:5) .eq. 'nine') then
        n = 9
      else
        n = 0
      end if

    end function

end program aoc1_part2
