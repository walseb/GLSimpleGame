module Render.SDL.Render where

import Control.Lens
import Linear
import FRPEngine.Render.SDL.Primitives
import qualified SDL as S
import FRPEngine.Types
import Types

render :: S.Renderer -> Resources -> (GameState) -> IO ()
render renderer res (GameState (CameraState zoomLevel) (PhysicalState player enemies) alive) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer

    case alive of
      True -> renderSpr (player ^. obj)
      False -> pure ()

    sequence_ $ renderSpr . (^. obj) <$> enemies

    S.present renderer
    pure ()
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (player ^. (obj . pos)) (flip getSprite res) zoomLevel renderer
    renderSpr = renderObj'
    -- renderTerr = renderObj' False
    -- renderText' = renderText renderer (res ^. font)
    renderPt pos = renderObj' (Obj pos 0 0 (V2 50 50) SobjectSprite2 True)
