library(plotly)

plot_cube_plan_vecteurs <- function(p1, q_list, plane_coef,
                                   pdv = list("x" = 1, "y" = -1.5, "z" = 1),
                                   affichage_cube = TRUE,
                                   affichage_hyperplan = TRUE,
                                   affichage_fleche = TRUE,
                                   ligne_point = NULL,
                                   ligne_vecteur = NULL,
                                   ligne_couleur = "red",
                                   ligne_epaisseur = 4) {
  # plane_coef = c(a,b,c) uniquement, plan : a x + b y + c z = d
  a <- plane_coef[1]; b <- plane_coef[2]; c <- plane_coef[3]
  if (all(c(a,b,c) == 0)) stop("Les coefficients a,b,c du plan ne peuvent pas être tous nuls.")
  
  # Calcul de d pour que plan passe par p1
  d <- a * p1[1] + b * p1[2] + c * p1[3]
  
  fig <- plot_ly()
  
  # Affichage du cube
  if (affichage_cube) {
    # Sommets cube
    x <- c(0,1,1,0,0,1,1,0)
    y <- c(0,0,1,1,0,0,1,1)
    z <- c(0,0,0,0,1,1,1,1)
    labels <- paste0("(", x, ",", y, ",", z, ")")
    
    faces <- list(
      c(1,2,3,4), c(5,6,7,8), c(1,2,6,5),
      c(2,3,7,6), c(3,4,8,7), c(4,1,5,8)
    )
    
    # Faces invisibles (opacity=0)
    for (f in faces) {
      fig <- fig %>%
        add_trace(
          type = 'mesh3d',
          x = x[f], y = y[f], z = z[f],
          opacity = 0,
          color = 'pink',
          flatshading = TRUE,
          showscale = FALSE
        )
    }
    
    # Arêtes noires pointillées
    edges <- list(
      c(1,2), c(2,3), c(3,4), c(4,1),
      c(5,6), c(6,7), c(7,8), c(8,5),
      c(1,5), c(2,6), c(3,7), c(4,8)
    )
    
    for (e in edges) {
      fig <- fig %>%
        add_trace(
          type = 'scatter3d',
          mode = 'lines',
          x = x[e], y = y[e], z = z[e],
          line = list(color = 'black', width = 3, dash = 'dash'),
          showlegend = FALSE
        )
    }
    
    # Labels sommets
    fig <- fig %>%
      add_trace(
        type = 'scatter3d',
        mode = 'text',
        x = x, y = y, z = z,
        text = labels,
        textposition = 'top center',
        showlegend = FALSE
      )
  }
  
  # Affichage de l'hyperplan
  if (affichage_hyperplan) {
    # Calcul de z du plan sur une grille en x,y
    grid <- seq(0, 1, length.out = 30)
    plane_points <- expand.grid(x = grid, y = grid)
    
    # Pour calculer z = (d - a x - b y) / c sauf si c=0
    if (c != 0) {
      plane_points$z <- (d - a * plane_points$x - b * plane_points$y) / c
      # garder points dans cube [0,1]^3
      plane_points <- subset(plane_points, z >= 0 & z <= 1)
    } else if (b != 0) {
      # si c=0, exprimer y en fonction de x,z
      grid_z <- seq(0,1,length.out=30)
      plane_points <- expand.grid(x=grid, z=grid_z)
      plane_points$y <- (d - a*plane_points$x - c*plane_points$z)/b
      plane_points <- subset(plane_points, y >= 0 & y <=1)
      plane_points <- plane_points[, c("x","y","z")]
    } else if (a != 0) {
      # si c=0 et b=0, exprimer x en fonction de y,z
      grid_y <- seq(0,1,length.out=30)
      grid_z <- seq(0,1,length.out=30)
      plane_points <- expand.grid(y=grid_y, z=grid_z)
      plane_points$x <- (d - b*plane_points$y - c*plane_points$z)/a
      plane_points <- subset(plane_points, x >= 0 & x <=1)
      plane_points <- plane_points[, c("x","y","z")]
    } else {
      stop("Plan invalide: au moins un de a,b,c doit être non nul")
    }
    
    fig <- fig %>%
      add_trace(
        type = 'mesh3d',
        x = plane_points$x, y = plane_points$y, z = plane_points$z,
        opacity = 0.6,
        color = 'blue',
        name = paste0("Plan ", a, "x + ", b, "y + ", c, "z = ", round(d, 2))
      )
  }
  
  # Axes + labels (toujours affichés)
  fig <- fig %>%
    add_trace(type='scatter3d', mode='lines+markers+text',
              x=c(0,1.2), y=c(0,0), z=c(0,0),
              line=list(color='black', width=2),
              marker=list(size=2, color='black'),
              text=c("","X"), textposition='top center',
              showlegend=FALSE) %>%
    add_trace(type='scatter3d', mode='lines+markers+text',
              x=c(0,0), y=c(0,1.2), z=c(0,0),
              line=list(color='black', width=2),
              marker=list(size=2, color='black'),
              text=c("","Y"), textposition='top center',
              showlegend=FALSE) %>%
    add_trace(type='scatter3d', mode='lines+markers+text',
              x=c(0,0), y=c(0,0), z=c(0,1.2),
              line=list(color='black', width=2),
              marker=list(size=2, color='black'),
              text=c("","Z"), textposition='top center',
              showlegend=FALSE)
  
  # Affichage d'une ligne (optionnel)
  if (!is.null(ligne_point) && !is.null(ligne_vecteur)) {
    # Normaliser le vecteur directeur
    norm_v <- sqrt(sum(ligne_vecteur^2))
    if (norm_v > 0) {
      v_unit <- ligne_vecteur / norm_v
      
      # Trouver les intersections de la ligne avec le cube [0,1]^3
      # Ligne paramétrée: P(t) = ligne_point + t * v_unit
      
      t_min <- -Inf
      t_max <- Inf
      
      # Pour chaque dimension, calculer les valeurs de t aux limites du cube
      for (i in 1:3) {
        if (abs(v_unit[i]) > 1e-10) {  # Eviter division par zéro
          t1 <- (0 - ligne_point[i]) / v_unit[i]  # intersection avec plan coordonnée = 0
          t2 <- (1 - ligne_point[i]) / v_unit[i]  # intersection avec plan coordonnée = 1
          
          t_enter <- min(t1, t2)
          t_exit <- max(t1, t2)
          
          t_min <- max(t_min, t_enter)
          t_max <- min(t_max, t_exit)
        } else {
          # Vecteur directeur perpendiculaire à cette dimension
          if (ligne_point[i] < 0 || ligne_point[i] > 1) {
            # La ligne ne passe pas dans le cube
            t_min <- Inf
            t_max <- -Inf
            break
          }
        }
      }
      
      # Si t_min <= t_max, la ligne intersecte le cube
      if (t_min <= t_max && t_min < Inf) {
        # Étendre un peu la ligne au-delà du cube pour la visibilité
        t_extend <- 0.2
        t_start <- t_min - t_extend
        t_end <- t_max + t_extend
        
        # Points de la ligne
        point_start <- ligne_point + t_start * v_unit
        point_end <- ligne_point + t_end * v_unit
        
        fig <- fig %>%
          add_trace(
            type = 'scatter3d',
            mode = 'lines',
            x = c(point_start[1], point_end[1]),
            y = c(point_start[2], point_end[2]),
            z = c(point_start[3], point_end[3]),
            line = list(color = ligne_couleur, width = ligne_epaisseur),
            showlegend = TRUE
          )
      }
    }
  }
  
  # Affichage des flèches
  if (affichage_fleche) {
    add_arrow <- function(fig, start, end, color) {
      dir <- end - start
      norm <- sqrt(sum(dir^2))
      if (norm == 0) return(fig)
      dir_unit <- dir / norm
      
      fig <- fig %>%
        add_trace(
          type = 'scatter3d',
          mode = 'lines',
          x = c(start[1], end[1]),
          y = c(start[2], end[2]),
          z = c(start[3], end[3]),
          line = list(color = color, width = 6),
          showlegend = FALSE
        ) %>%
        add_trace(
          type = "cone",
          x = end[1] - 0.05 * dir_unit[1],
          y = end[2] - 0.05 * dir_unit[2],
          z = end[3] - 0.05 * dir_unit[3],
          u = dir_unit[1],
          v = dir_unit[2],
          w = dir_unit[3],
          sizemode = "absolute",
          sizeref = 0.1,
          colorscale = list(c(0, 1), c(color, color)),
          showscale = FALSE
        )
      fig
    }
    
    # Flèche 0->p1 violet
    fig <- add_arrow(fig, c(0,0,0), p1, "purple")
    
    current_point <- p1
    for (q in q_list) {
      next_point <- current_point + q
      fig <- add_arrow(fig, current_point, next_point, "orange")
      current_point <- next_point
    }
  }
  
  fig <- fig %>%
    layout(scene = list(
      aspectmode = 'cube',
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      zaxis = list(visible = FALSE)
    ))
  
  fig <- fig %>% layout(scene = list(
    camera = list(
      eye = list(x = pdv$x, y = pdv$y, z = pdv$z)
    )
  ))
}


# Exemple d'utilisation avec les nouvelles options :
# p1 <- c(2/3, 2/3, 2/3)
# q1 <- c(0.1, -0.2, 0.3)
# q2 <- c(-0.3, 0.4, 0)
# q3 <- c(0.2, 0.1, -0.1)
# plane_normal <- c(1, 1, 1)
# 
# # Définir une ligne : point de passage et vecteur directeur
# point_ligne <- c(0.2, 0.3, 0.1)
# vecteur_directeur <- c(1, 0.5, 2)
# 
# # Afficher avec une ligne rouge
# f1 <- plot_cube_plan_vecteurs(p1, list(q1, q2, q3), plane_normal, 
#                               ligne_point = point_ligne, 
#                               ligne_vecteur = vecteur_directeur,
#                               ligne_couleur = "red")
# 
# # Afficher seulement le cube et la ligne (sans hyperplan ni flèches)
# f2 <- plot_cube_plan_vecteurs(p1, list(q1, q2, q3), plane_normal, 
#                               affichage_hyperplan = FALSE, 
#                               affichage_fleche = FALSE,
#                               ligne_point = point_ligne, 
#                               ligne_vecteur = vecteur_directeur,
#                               ligne_couleur = "green",
#                               ligne_epaisseur = 6)
# 
# # Affichage complet (par défaut)
# f3 <- plot_cube_plan_vecteurs(p1, list(q1, q2, q3), plane_normal)