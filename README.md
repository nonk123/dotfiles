# dotfiles

🫃 behavior dotfiles powered by [chezmoi](https://www.chezmoi.io).

TODO: document contents and usage.

## Random notes I have nowhere else to jot down

### Linux kernel EFI boot stub

[Booting without a dedicated bootloader](https://wiki.archlinux.org/title/EFI_boot_stub#efibootmgr) such as GRUB:

```sh
sudo efibootmgr --create \
 --disk /dev/sda --part 1 \
 --label 'Arch Linux' \
 --loader '\vmlinuz-linux' \
 --unicode 'root=UUID=1446fff4-91d0-48e2-bc13-51471809dafd rw loglevel=3 nowatchdog initrd=\initramfs-linux.img nvidia-drm.modeset=1'
```

Modify the `root=UUID=` part to match the UUID of your root partition, e.g.:

```sh
$ lsblk -f
NAME   FSTYPE FSVER LABEL UUID                                 FSAVAIL FSUSE% MOUNTPOINTS
sda
├─sda1 vfat   FAT32       5DE0-FFF7                               313M    39% /boot
└─sda2 ext4   1.0         1446fff4-91d0-48e2-bc13-51471809dafd  419,7G     5% /
zram0  swap   1     zram0 acbdc2a4-c431-456e-bf5d-713ce799040f                [SWAP]
```

And change `initramfs-linux.img` & `vmlinuz-linux` if you are using a different kernel image.

### Google AI's Take on Win7 Cursors

```sh
ln -fs $PWD/.src/aero_arrow default
ln -fs $PWD/.src/aero_arrow left_ptr
ln -fs $PWD/.src/aero_arrow arrow
ln -fs $PWD/.src/aero_arrow top_left_arrow
ln -fs $PWD/.src/aero_arrow draft_small
ln -fs $PWD/.src/aero_beam xterm
ln -fs $PWD/.src/aero_beam text
ln -fs $PWD/.src/aero_beam ibeam
ln -fs $PWD/.src/aero_busy watch
ln -fs $PWD/.src/aero_busy progress
ln -fs $PWD/.src/aero_busy 08e8e1c95fe2fc01f976f1e063a24ccd
ln -fs $PWD/.src/aero_cross cross
ln -fs $PWD/.src/aero_cross crosshair
ln -fs $PWD/.src/aero_cross tcross
ln -fs $PWD/.src/aero_help help
ln -fs $PWD/.src/aero_help question_arrow
ln -fs $PWD/.src/aero_help whats_this
ln -fs $PWD/.src/aero_help dnd-ask
ln -fs $PWD/.src/aero_link hand
ln -fs $PWD/.src/aero_link hand2
ln -fs $PWD/.src/aero_link pointer
ln -fs $PWD/.src/aero_link dnd-link
ln -fs $PWD/.src/aero_move move
ln -fs $PWD/.src/aero_move fleur
ln -fs $PWD/.src/aero_move dnd-move
ln -fs $PWD/.src/aero_no crossed_circle
ln -fs $PWD/.src/aero_no forbidden
ln -fs $PWD/.src/aero_no not-allowed
ln -fs $PWD/.src/aero_no dnd-none
ln -fs $PWD/.src/aero_pen pencil
ln -fs $PWD/.src/aero_pen draft
ln -fs $PWD/.src/aero_size1 sb_v_double_arrow
ln -fs $PWD/.src/aero_size1 v_double_arrow
ln -fs $PWD/.src/aero_size1 n-resize
ln -fs $PWD/.src/aero_size1 s-resize
ln -fs $PWD/.src/aero_size2 sb_h_double_arrow
ln -fs $PWD/.src/aero_size2 h_double_arrow
ln -fs $PWD/.src/aero_size2 e-resize
ln -fs $PWD/.src/aero_size2 w-resize
ln -fs $PWD/.src/aero_size3 bd_double_arrow
ln -fs $PWD/.src/aero_size3 ne-resize
ln -fs $PWD/.src/aero_size3 sw-resize
ln -fs $PWD/.src/aero_size4 fd_double_arrow
ln -fs $PWD/.src/aero_size4 nw-resize
ln -fs $PWD/.src/aero_size4 se-resize
ln -fs $PWD/.src/aero_up center_ptr
ln -fs $PWD/.src/aero_working left_ptr_watch
```
