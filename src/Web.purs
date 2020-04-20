module Phutball.Web where

import Prelude
import CSS (black, border, height, px, solid, width)
import Data.Array ((..))
import Data.Char (fromCharCode)
import Data.Const (Const)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (HTML, table_, td, text, th_, tr_)
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)

component :: Component HTML (Const Void) Unit Void Aff
component =
  mkComponent
    { initialState: initialState
    , render: render
    , eval: eval
    }
  where
  initialState :: Unit -> Unit
  initialState = const unit

  render :: Unit -> ComponentHTML Unit () Aff
  render _ =
    table_
      $ (renderRow <$> 19 .. 1)
      <> [ tr_
            $ [ th_ [] ]
            <> ( (\c -> th_ [ text $ singleton $ fromMaybe ' ' $ fromCharCode $ 65 + c ])
                  <$> (0 .. 15)
              )
        ]
    where
    renderCell r c =
      td
        [ style do
            width $ px 25.0
            height $ px 25.0
            border solid (px 1.0) black
        ]
        []

    -- [ text $ show r <> "/" <> show c ]
    renderRow r = tr_ $ [ th_ [ text $ show r ] ] <> (renderCell r <$> 0 .. 15)

  eval = mkEval defaultEval

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    void $ runUI component unit body
