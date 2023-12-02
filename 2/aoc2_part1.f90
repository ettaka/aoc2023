program aoc2_part1
  implicit none
  character(156) :: line
  integer :: stat, answer=0

  open(10, file='input.txt', status='old', iostat=stat)

  main_loop: do while (.true.)
    block
      integer :: i, game_number=0, games_start=0
      character(10) :: ch
      logical :: possible

      read(10, '(A)', end=99) line

      game_loop: do i=1,len_trim(line)
        if (line(i:i) .eq. ":") then
          games_start = i+1
          read (line(1:i-1),'(A5,I3)') ch, game_number
          exit game_loop
        end if
      end do game_loop

!      print *, "line:", line
!      print *, "game", game_number

      block
        integer :: n_balls

        possible = .true.
        possibility_loop: do i=games_start, len_trim(line)
          if (line(i:i) .eq. 'r') then
            read (line(i-3:i-1), '(I2)') n_balls
            if (n_balls .gt. 12) then
              possible = .false.
              exit possibility_loop
            end if
          else if (line(i:i) .eq. 'g') then
            read (line(i-3:i-1), '(I2)') n_balls
            if (n_balls .gt. 13) then
              possible = .false.
              exit possibility_loop
            end if
          else if (line(i:i) .eq. 'b') then
            read (line(i-3:i-1), '(I2)') n_balls
            if (n_balls .gt. 14) then
              possible = .false.
              exit possibility_loop
            end if
          end if
        end do possibility_loop
      end block

      if (possible) then
!        print *, "game is possible"
        answer=answer+game_number
      else
!        print *, "game is impossible"
      end if
    end block
  end do main_loop

  99 continue
  close(10)

  print *, answer
end program aoc2_part1
