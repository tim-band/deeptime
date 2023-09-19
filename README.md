# deeptime
Website with which to view the entire history of the universe

# Using GPlates to make geography data

In GPlates, choose File|Manage Feature Collections (Ctrl+M).
Choose Open File... and get into the standard data supplied with GPlates.
Open the file FeatureCollections/Paleogeography/Gloval/Scotese_PALEOMAP/Map1a PALEOMAP PaleoAtlas.gpml

Now choose a projection with View|Set Projection... (I choose Robinson)

Now choose Reconstruction|Export...
and you can set 410-0 by 10.
Height should be chosen to be about half of width.
Then choose an output direcory and Add Export..., Image (screenshot), .jpg, and template `image_%d`.
