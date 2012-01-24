module Main where
import Graphics.UI.WX

-- constantes : taille des case du plateau et des pions, positionnement des balles
Tcase, maxX, maxY, maxH, radius :: Int
Tcase = 25
maxY = Tcase * 6
maxX = Tcase * 7
maxH = maxY
radius = 10
--the main function
main = start puissance4

puissance4
  = do
       game <- varCreate []

       -- creer la fenetre
       f <- frameFixed [text := "Puissance 4"]

       -- creer une zone de dessin
       p <- panel f [on paint := jeuEnCours game]

       -- creer un timer pour mettre a jour le plateau
       t <- timer f [interval := 20, on command := miseAJour game p]

       -- detecter le clic d'un joueur
       set p [on click         := jouerCoup game p]

       -- put the panel in the frame, with a minimal size
       set f [layout := minsize (sz maxX maxY) $ widget p]
	   
	   bNewGame <- button f [ text := "Nouvelle Partie", on command := newTable]
	where
	-- dessiner les pions
    jeuEnCours :: Var [[table]] -> DC a -> Rect -> IO ()
    jeuEnCours game dc viewArea
      = do pions <- varGet grille -- a modifier pour detecter la couleur des pions
           set dc [brushColor := red, brushKind := BrushSolid]
           mapM_ (drawPion dc) [p | (p:ps) <- grille]

    drawPion dc pt
      = circle dc pt radius []
	  
	-- mettre afficher les nouveaux pions 
    miseAJour :: Var [[Point]] -> Panel () -> IO ()
    miseAJour game p
      = do varUpdate game
           repaint p
		   
	-- Recuperer la position du coup
    jouerCoup :: Var [[Point]] -> Panel () -> Point -> IO ()
    jouerCoup game p pt
      = do varUpdate game (placerCoup pt:)
           repaint p

    -- jouer le coup
    placerCoup (Point x)
      = -- non complete