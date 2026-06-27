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
