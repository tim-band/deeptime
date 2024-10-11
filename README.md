# deeptime
Website with which to view the entire history of the universe

# deploy deeptime

Begin by building deeptime, then rsync it to the target machine, then go
onto the machine and rsync or link this code ti /var/www/html/deeptime. Let's use the example of linuxmac:

```sh
elm make src/Main.elm
rsync index.html linuxmac:deeptime
rsync -r resources/ linuxmac:deeptime/resources
ssh linuxmac
sudo rsync -r deeptime/ /var/www/html/deeptime/
exit
```

Then as long as `/var/www/html` is visible in your server you should be
able to see Deep TIme at, for example, `<server>/deeptime/index.html`.

# Using GPlates to make geography data

In GPlates, choose File|Manage Feature Collections (Ctrl+M).
Choose Open File... and get into the standard data supplied with GPlates.
Open the file FeatureCollections/Paleogeography/Gloval/Scotese_PALEOMAP/Map1a PALEOMAP PaleoAtlas.gpml

Now choose a projection with View|Set Projection... (I choose Robinson)

Now choose Reconstruction|Export...
and you can set 410-0 by 10.
Height should be chosen to be about half of width.
Then choose an output direcory and Add Export..., Image (screenshot), .jpg, and template `image_%d`.
