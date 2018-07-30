{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Duta.Web where

import Duta.Web.Controllers
import Duta.Web.Foundation
import Duta.Web.Types
import Yesod hiding (toHtml, Html)
import Yesod.Auth

mkYesodDispatch "App" resourcesApp
