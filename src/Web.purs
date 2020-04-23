module Phutball.Web where

import Prelude
import CSS (black, border, height, px, solid, width)
import Data.Array ((..))
import Data.Const (Const)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (HTML, table_, td, text, th, tr_)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events (onClick)
import Halogen.VDom.Driver (runUI)
import Phutball.Utils (toLetter)

data Action
  = Select Int Int

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

  render :: Unit -> ComponentHTML Action () Aff
  render _ =
    table_
      $ (renderRow <$> 19 .. 1)
      <> [ lastRow ]
    where
    renderCell r c =
      td
        [ onClick \_ -> pure $ Select r c
        , style do
            width $ px 25.0
            height $ px 25.0
            border solid (px 1.0) black
        ]
        []

    renderHeader :: forall w i. String -> HTML w i
    renderHeader t =
      th
        [ style do
            width $ px 25.0
            height $ px 25.0
        ]
        [ text t ]

    renderRow r =
      tr_
        ( [ renderHeader $ show r ]
            <> (renderCell r <$> 0 .. 15)
        )

    lastRow =
      tr_
        ( [ renderHeader "" ]
            <> (renderLetterHeaderCell <$> (0 .. 15))
        )

    renderLetterHeaderCell :: forall w i. Int -> HTML w i
    renderLetterHeaderCell = renderHeader <<< fromMaybe "" <<< toLetter

  handleAction :: Action -> HalogenM Unit Action () Void Aff Unit
  handleAction (Select r c) = do
    liftEffect $ log $ "Select " <> show [ r, c ]

  eval = mkEval (defaultEval { handleAction = handleAction })

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    void $ runUI component unit body
